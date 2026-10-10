import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Services.Notifications

Scope {
    id: root

    // Maps a tracked notification's id to how many duplicates/tag-updates it has
    // absorbed, so the bubble can show a dunst-style "(N)" counter.
    property var dupCounts: ({})

    // Set for the duration of a stacking replacement (dismiss old + track
    // new, meant to feel like one continuously-updating bubble) so the
    // add/remove fade is skipped and it swaps instantly instead.
    property bool suppressBubbleAnim: false

    // How many notifications are shown before the oldest normal one gets
    // evicted to make room, independent of how much screen space is available.
    readonly property int maxVisibleBubbles: 5
    readonly property int bubbleSpacing: 12

    NotificationServer {
        id: server
        bodySupported: true
        bodyMarkupSupported: true
        bodyHyperlinksSupported: true
        actionsSupported: true
        imageSupported: true
        persistenceSupported: false
        keepOnReload: false
        extraHints: ["value", "x-dunst-stack-tag"]

        onNotification: (notification) => {
            // Every notification is logged to history regardless of DND, so
            // nothing is lost even when bubbles are suppressed.
            NotificationHistory.record(notification)

            if (NotificationHistory.dndEnabled) return

            // Mimic dunst's stacking: same app + tag (x-dunst-stack-tag, or
            // the GNOME-style x-canonical-private-synchronous used by
            // volume/brightness OSD scripts), or same app + summary + body,
            // replaces the previous bubble and bumps a "(N)" counter
            // instead of piling up a new one. Also covers scripts relying
            // on a hardcoded --replace-id, since a client-picked id never
            // matches a real server-assigned one and so never replaces.
            const tag = notification.hints["x-dunst-stack-tag"] || notification.hints["x-canonical-private-synchronous"]

            let existing = null
            for (const n of server.trackedNotifications.values) {
                if (n.appName !== notification.appName) continue
                const isMatch = tag
                    ? (n.hints["x-dunst-stack-tag"] || n.hints["x-canonical-private-synchronous"]) === tag
                    : (n.summary === notification.summary && n.body === notification.body)
                if (isMatch) {
                    existing = n
                    break
                }
            }

            if (existing) {
                const count = (root.dupCounts[existing.id] || 1) + 1
                root.dupCounts = Object.assign({}, root.dupCounts, { [notification.id]: count })
                root.suppressBubbleAnim = true
                existing.dismiss()
            } else if (server.trackedNotifications.values.length >= root.maxVisibleBubbles) {
                // A genuinely new notification (not a stacking replacement)
                // would overflow the fixed-height stack: evict the oldest
                // (topmost) one that isn't urgent or explicitly persistent
                // to make room, same as dunst's notification_limit.
                for (const n of server.trackedNotifications.values) {
                    if (n.urgency === NotificationUrgency.Critical) continue
                    if (n.expireTimeout === 0) continue
                    n.dismiss()
                    break
                }
            }

            notification.tracked = true

            // Give the view a moment to process the swap before re-enabling
            // animations for genuinely new/unrelated bubbles.
            if (existing) Qt.callLater(() => { root.suppressBubbleAnim = false })
        }
    }

    IpcHandler {
        target: "notifications"

        function closeAll(): void {
            // dismiss() removes from trackedNotifications.values as it goes, so
            // iterate a snapshot instead of the live list or entries get skipped.
            for (const n of [...server.trackedNotifications.values]) n.dismiss()
        }
    }

    PanelWindow {
        WlrLayershell.namespace: "quickshell-notifications"

        screen: {
            for (let i = 0; i < Quickshell.screens.length; i++) {
                if (Quickshell.screens[i].name === "DP-3") return Quickshell.screens[i]
            }
            return Quickshell.screens[0]
        }

        anchors {
            top: true
            right: true
        }

        margins {
            top: 5
            right: 5
        }

        exclusiveZone: 0
        color: "transparent"

        // Without this, the whole implicitWidth x implicitHeight surface
        // claims Wayland input - a transparent 300px-wide strip down most
        // of the screen height eating clicks even with zero bubbles on
        // screen. Mask down to just the vertical span the bubbles actually
        // occupy (0 when the list is empty), tracking contentHeight live.
        mask: Region {
            x: 0
            y: 0
            width: bubbleList.width
            height: Math.min(bubbleList.contentHeight, bubbleList.height)
        }

        implicitWidth: 300
        // Fixed, not bound to bubbleList.contentHeight: this drives a real
        // Wayland surface resize, and the compositor repaints the whole
        // surface on every resize - which flashed every bubble, not just
        // the one that changed. Maxed to the screen height so tall bubbles
        // always have room; eviction above caps the count regardless.
        // Stops short of the full screen height (barClearance) so this
        // transparent, always-mapped surface never reaches down into the
        // bar's row - it stacks above Bar's own window (created later in
        // shell.qml) and was silently eating clicks meant for the bar's
        // right-hand widgets in that overlap, despite painting invisible.
        readonly property int barClearance: 40
        implicitHeight: screen.height - margins.top - barClearance

        ListView {
            id: bubbleList
            width: parent.width
            height: parent.height
            clip: true
            interactive: false
            spacing: root.bubbleSpacing
            model: server.trackedNotifications

            // Same motion language as the notification center's history
            // list: fade in/out per bubble, and slide the rest smoothly
            // into place when one above/below them is added or removed.
            addDisplaced: Transition {
                NumberAnimation { properties: "y"; duration: 220; easing.type: Easing.OutCubic }
            }
            removeDisplaced: Transition {
                NumberAnimation { properties: "y"; duration: 220; easing.type: Easing.OutCubic }
            }

            delegate: Rectangle {
                id: bubble
                required property Notification modelData

                // delayRemove keeps this delegate alive to animate its
                // exit, but Quickshell destroys the Notification itself the
                // instant it's dismissed - so every binding below reads
                // these captured copies rather than modelData directly,
                // to avoid dereferencing a destroyed object mid-fade.
                property string capturedAppName: ""
                property string capturedSummary: ""
                property string capturedBody: ""
                property string capturedIcon: ""
                property int capturedUrgency: NotificationUrgency.Normal
                property real capturedProgress: -1
                // Plain strings, not the live NotificationAction objects -
                // those are children of the Notification and get destroyed
                // with it, which shrank the button box to fit its
                // now-empty text mid-fade.
                property var capturedActionLabels: []
                property int capturedDupCount: 1
                property int capturedTimeoutMs: 10000

                function _resolveIcon(n) {
                    const rawPath = n.hints["image-path"] || n.hints["image_path"] || ""
                    const icon = n.appIcon || rawPath || n.image
                    if (!icon) return ""
                    if (icon.startsWith("/")) return "file://" + icon
                    if (icon.startsWith("file://") || icon.startsWith("http") || icon.startsWith("image://")) return icon
                    return Quickshell.iconPath(icon)
                }

                function _resolveProgress(n) {
                    const v = n.hints["value"]
                    if (v === undefined || v === "") return -1
                    const num = Number(v)
                    if (isNaN(num)) return -1
                    return Math.max(0, Math.min(100, num))
                }

                function capture() {
                    const n = bubble.modelData
                    if (!n) return
                    capturedAppName = n.appName
                    capturedSummary = n.summary
                    capturedBody = n.body
                    capturedIcon = bubble._resolveIcon(n)
                    capturedUrgency = n.urgency
                    capturedProgress = bubble._resolveProgress(n)
                    capturedActionLabels = NotificationHistory.visibleActions(n).map(a => NotificationHistory.cleanActionLabel(a))
                    capturedDupCount = root.dupCounts[n.id] || 1
                    // Per spec, expireTimeout is -1 when the app leaves it up to the
                    // server, 0 when the app asks to never expire, and >0 for an
                    // explicit duration. Only the -1 case falls back to dunstrc's
                    // per-urgency default (urgency_low/normal = 10s, critical = never).
                    capturedTimeoutMs = n.expireTimeout >= 0
                        ? n.expireTimeout
                        : (n.urgency === NotificationUrgency.Critical ? 0 : 10000)
                }
                // A null modelData (mid fade-out) leaves the snapshot in
                // place on purpose; Component.onCompleted also covers
                // bubbles present when the ListView first appears, since
                // ListView.onAdd only fires for genuine insertions.
                onModelDataChanged: bubble.capture()
                Component.onCompleted: {
                    bubble.capture()
                    bubble.opacity = 1
                }

                width: ListView.view.width
                implicitHeight: content.implicitHeight + 32

                color: Colors.surfaceContainerLowest
                radius: 0
                border.width: 3
                border.color: bubble.capturedUrgency === NotificationUrgency.Critical
                    ? Colors.error
                    : Colors.surfaceContainerLowest

                // Fade in on arrival - skipped for a stacking replacement,
                // which should feel like an instant in-place update.
                opacity: 0
                NumberAnimation {
                    id: addAnimation
                    target: bubble
                    property: "opacity"
                    from: 0
                    to: 1
                    duration: 180
                    easing.type: Easing.OutCubic
                }
                ListView.onAdd: {
                    if (root.suppressBubbleAnim) bubble.opacity = 1
                    else addAnimation.start()
                }

                // Fade out before leaving the model; removeDisplaced above
                // slides the rest into place. Skipped for a stacking
                // replacement (suppressBubbleAnim) - delayRemove would
                // otherwise hold this delegate alive past the point its
                // Notification gets destroyed.
                SequentialAnimation {
                    id: removeAnimation
                    PropertyAction { target: bubble; property: "ListView.delayRemove"; value: true }
                    NumberAnimation { target: bubble; property: "opacity"; to: 0; duration: 150; easing.type: Easing.InCubic }
                    PropertyAction { target: bubble; property: "ListView.delayRemove"; value: false }
                }
                ListView.onRemove: if (!root.suppressBubbleAnim) removeAnimation.start()

                // Declared before `content` so it sits underneath: the
                // action buttons inside `content` (added after, on top)
                // get first claim on clicks within their own bounds,
                // while everywhere else on the bubble falls through to
                // this general click-catcher.
                MouseArea {
                    anchors.fill: parent
                    acceptedButtons: Qt.LeftButton | Qt.MiddleButton | Qt.RightButton
                    onClicked: (mouse) => {
                        const n = bubble.modelData
                        if (!n) return
                        if (mouse.button === Qt.LeftButton) {
                            // Clicking the bubble body is the conventional
                            // way to invoke the implicit "default" action
                            // (e.g. Heroic's "open game"), which isn't
                            // rendered as its own button. Unlike the action
                            // chips, this is also the explicit "click
                            // elsewhere to dismiss" gesture - always closes,
                            // even for resident notifications, since resident
                            // only protects against auto-close *because an
                            // action fired*, not against the user closing it.
                            const def = NotificationHistory.defaultAction(n)
                            if (def) def.invoke()
                            n.dismiss()
                        } else if (mouse.button === Qt.MiddleButton) {
                            const first = NotificationHistory.visibleActions(n)[0]
                            if (first) first.invoke()
                            if (!n.resident) n.dismiss()
                        } else if (mouse.button === Qt.RightButton) {
                            for (const t of [...server.trackedNotifications.values]) t.dismiss()
                        }
                    }
                }

                RowLayout {
                    id: content
                    anchors.fill: parent
                    anchors.margins: 16
                    spacing: 12

                    Image {
                        visible: bubble.capturedIcon.length > 0
                        Layout.preferredWidth: visible ? 32 : 0
                        Layout.preferredHeight: 32
                        Layout.alignment: Qt.AlignVCenter
                        source: bubble.capturedIcon
                        fillMode: Image.PreserveAspectFit
                    }

                    ColumnLayout {
                        Layout.fillWidth: true
                        spacing: 4

                        Text {
                            visible: text.length > 0
                            Layout.fillWidth: true
                            text: bubble.capturedAppName
                            font.family: Style.fontFamily
                            font.pixelSize: Style.fontSize - 2
                            color: Colors.outline
                            wrapMode: Text.Wrap
                            elide: Text.ElideRight
                            maximumLineCount: 1
                        }

                        Text {
                            Layout.fillWidth: true
                            text: bubble.capturedSummary + (bubble.capturedProgress < 0 && bubble.capturedDupCount > 1 ? " (" + bubble.capturedDupCount + ")" : "")
                            font.bold: true
                            font.family: Style.fontFamily
                            font.pixelSize: Style.fontSize
                            color: Colors.onSurfaceVariant
                            wrapMode: Text.Wrap
                            elide: Text.ElideRight
                            maximumLineCount: 2
                        }

                        Text {
                            visible: text.length > 0
                            Layout.fillWidth: true
                            text: bubble.capturedBody
                            textFormat: Text.RichText
                            font.family: Style.fontFamily
                            font.pixelSize: Style.fontSize
                            color: Colors.onSurfaceVariant
                            wrapMode: Text.Wrap
                            elide: Text.ElideRight
                            maximumLineCount: 5
                        }

                        Rectangle {
                            id: progressTrack
                            visible: bubble.capturedProgress >= 0
                            Layout.fillWidth: true
                            Layout.preferredHeight: 4
                            Layout.topMargin: 2
                            radius: 0
                            color: Colors.surfaceContainerHigh

                            Rectangle {
                                anchors.left: parent.left
                                anchors.top: parent.top
                                anchors.bottom: parent.bottom
                                radius: 0
                                color: Colors.primary
                                width: parent.width * (bubble.capturedProgress / 100)
                            }
                        }

                        RowLayout {
                            visible: bubble.capturedActionLabels.length > 0
                            Layout.topMargin: 10
                            spacing: 10

                            Repeater {
                                model: bubble.capturedActionLabels

                                Rectangle {
                                    id: actionChip
                                    required property string modelData
                                    required property int index
                                    radius: 0
                                    color: actionMouse.containsMouse ? Colors.surfaceContainerHigh : Colors.surfaceContainerLowest
                                    border.width: 1
                                    border.color: Colors.primary
                                    Behavior on color { ColorAnimation { duration: 120 } }
                                    implicitWidth: actionLabel.implicitWidth + 14
                                    implicitHeight: actionLabel.implicitHeight + 8

                                    Text {
                                        id: actionLabel
                                        anchors.centerIn: parent
                                        text: actionChip.modelData
                                        color: Colors.primary
                                        font.family: Style.fontFamily
                                        font.pixelSize: Style.fontSize - 2
                                    }

                                    MouseArea {
                                        id: actionMouse
                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: {
                                            const n = bubble.modelData
                                            if (!n) return
                                            const action = NotificationHistory.visibleActions(n)[actionChip.index]
                                            if (action) action.invoke()
                                            if (!n.resident) n.dismiss()
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                Timer {
                    running: bubble.capturedTimeoutMs > 0
                    interval: bubble.capturedTimeoutMs
                    onTriggered: if (bubble.modelData) bubble.modelData.expire()
                }
            }
        }
    }
}
