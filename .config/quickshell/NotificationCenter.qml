import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland

Item {
    id: root
    implicitWidth: rowLayout.implicitWidth
    implicitHeight: bellText.implicitHeight

    property bool detailOpen: false
    // Set for the duration of "Clear all"'s bulk clear so each row's own
    // remove animation (which fires even for a plain ListModel.clear()) is
    // skipped in favor of the single whole-area fade below - both playing
    // at once left stale items flickering back into view mid-fade-in.
    property bool clearingAll: false

    // Bar.qml's own PanelWindow, passed in so the popup can track this
    // widget's actual position - see card.x/y below.
    property var barWindow: null

    function openPanel() {
        NotificationHistory.markAllRead()
        root.detailOpen = true
    }

    function relativeTime(ms) {
        const diff = Math.floor((Date.now() - ms) / 1000)
        if (diff < 60) return "now"
        if (diff < 3600) return Math.floor(diff / 60) + "m"
        if (diff < 86400) return Math.floor(diff / 3600) + "h"
        return Math.floor(diff / 86400) + "d"
    }

    RowLayout {
        id: rowLayout
        anchors.fill: parent
        spacing: 4

        Text {
            id: bellText
            text: NotificationHistory.dndEnabled ? "󰂛" : "󰂚"
            color: Colors.onSurfaceVariant
            font.family: Style.fontFamily
            font.pixelSize: Style.fontSize
        }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: (mouse) => {
            if (mouse.button === Qt.LeftButton) {
                if (root.detailOpen) root.detailOpen = false
                else root.openPanel()
            } else if (mouse.button === Qt.RightButton) {
                NotificationHistory.toggleDnd()
            }
        }
    }

    // Full-screen overlay (not edge-anchored) so clicking anywhere outside
    // the card can dismiss it; the card itself is positioned manually,
    // same pattern as Clock.qml's detail card.
    PanelWindow {
        id: overlay
        WlrLayershell.namespace: "quickshell-notification-center"
        WlrLayershell.layer: WlrLayer.Overlay

        screen: {
            for (let i = 0; i < Quickshell.screens.length; i++) {
                if (Quickshell.screens[i].name === "DP-3") return Quickshell.screens[i]
            }
            return Quickshell.screens[0]
        }

        anchors { top: true; bottom: true; left: true; right: true }
        exclusionMode: ExclusionMode.Ignore
        color: "transparent"

        visible: root.detailOpen

        MouseArea {
            anchors.fill: parent
            onClicked: root.detailOpen = false
        }

        Rectangle {
            id: card
            readonly property int padding: 12
            // Live bindings, not a one-shot computation: overlay.height in
            // particular isn't negotiated by the compositor until this
            // (normally hidden) surface actually becomes visible, so a
            // snapshot taken right before that would freeze in a stale
            // placeholder value forever. `void root.detailOpen` has no
            // effect on the value but forces x to re-read mapToItem every
            // time the panel opens, since nothing else would otherwise
            // prompt that (unlike y, which naturally redoes itself whenever
            // overlay.height changes). Centered under the widget, clamped
            // so it never runs past the screen edges.
            x: {
                void root.detailOpen
                if (!root.barWindow) return 0
                const widgetCenterX = root.barWindow.margins.left + root.mapToItem(null, 0, 0).x + root.width / 2
                return Math.max(0, Math.min(widgetCenterX - width / 2, overlay.width - width))
            }
            y: root.barWindow ? (overlay.height - (root.barWindow.margins.bottom + root.barWindow.height) - height - 6) : 0
            width: 340
            height: 460
            color: Colors.surfaceContainerLowest
            border.width: 0
            border.color: Colors.outline

            // Swallows clicks so they don't fall through to the dismiss
            // MouseArea behind the card.
            MouseArea {
                anchors.fill: parent
            }

            ColumnLayout {
                anchors.fill: parent
                anchors.margins: card.padding
                spacing: 8

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 12

                    Text {
                        Layout.fillWidth: true
                        text: "Notifications"
                        font.bold: true
                        color: Colors.onSurfaceVariant
                        font.family: Style.fontFamily
                        font.pixelSize: Style.fontSize
                    }

                    Rectangle {
                        readonly property bool hasEntries: NotificationHistory.historyModel.count > 0
                        opacity: hasEntries ? 1 : 0.4
                        radius: 0
                        color: clearMouse.containsMouse ? Colors.surfaceContainerHigh : "transparent"
                        Behavior on color { ColorAnimation { duration: 120 } }
                        implicitWidth: clearLabel.implicitWidth + 12
                        implicitHeight: clearLabel.implicitHeight + 6

                        Text {
                            id: clearLabel
                            anchors.centerIn: parent
                            text: "Clear all"
                            color: Colors.outline
                            font.family: Style.fontFamily
                            font.pixelSize: Style.fontSize - 2
                        }

                        MouseArea {
                            id: clearMouse
                            enabled: parent.hasEntries
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: clearAllAnim.start()
                        }
                    }

                    Rectangle {
                        radius: 0
                        color: dndMouse.containsMouse ? Colors.surfaceContainerHigh : "transparent"
                        Behavior on color { ColorAnimation { duration: 120 } }
                        implicitWidth: 22
                        implicitHeight: 22

                        Text {
                            anchors.centerIn: parent
                            text: NotificationHistory.dndEnabled ? "󰂛" : "󰂚"
                            color: NotificationHistory.dndEnabled ? Colors.error : Colors.outline
                            font.family: Style.fontFamily
                            font.pixelSize: Style.fontSize + 2
                        }

                        MouseArea {
                            id: dndMouse
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: NotificationHistory.toggleDnd()
                        }
                    }
                }

                // A single fixed-size area hosting both the list and the
                // empty-state message, so switching between them never
                // reflows the surrounding ColumnLayout and "Clear all" can
                // cross-fade the two as one unit instead of the empty text
                // popping in at full opacity mid-transition.
                Item {
                    id: historyArea
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    // ListView's per-item remove transitions aren't suited
                    // to clearing everything at once (several delegates in
                    // delayRemove limbo makes for janky geometry), so
                    // "Clear all" cross-fades this whole area instead.
                    SequentialAnimation {
                        id: clearAllAnim
                        ScriptAction { script: root.clearingAll = true }
                        NumberAnimation { target: historyArea; property: "opacity"; to: 0; duration: 150; easing.type: Easing.InCubic }
                        ScriptAction { script: NotificationHistory.clearAll() }
                        NumberAnimation { target: historyArea; property: "opacity"; to: 1; duration: 150; easing.type: Easing.OutCubic }
                        ScriptAction { script: root.clearingAll = false }
                    }

                    Text {
                        visible: NotificationHistory.historyModel.count === 0
                        anchors.centerIn: parent
                        text: "No notifications"
                        color: Colors.outline
                        font.family: Style.fontFamily
                        font.pixelSize: Style.fontSize
                    }

                    ListView {
                        id: historyList
                        anchors.fill: parent
                        clip: true
                        spacing: 6

                        // The default overshoot/rebound bounce is a touch-flick
                        // affordance; it just looks janky driven by a mouse
                        // wheel, so stop hard at the bounds instead.
                        boundsBehavior: Flickable.StopAtBounds
                        model: NotificationHistory.historyModel

                        // Items sliding to a new position (because one above them
                        // was added/removed) animate smoothly instead of jumping.
                        // Qt's docs recommend the specific add/removeDisplaced
                        // over the generic `displaced` for reliable behavior.
                        addDisplaced: Transition {
                            NumberAnimation { properties: "y"; duration: 220; easing.type: Easing.OutCubic }
                        }
                        removeDisplaced: Transition {
                            NumberAnimation { properties: "y"; duration: 220; easing.type: Easing.OutCubic }
                        }

                        delegate: Rectangle {
                            id: entry
                            required property int historyId
                            required property real time
                            required property string appName
                            required property string icon
                            required property string summary
                            required property string body
                            required property int urgency
                            required property bool read
                            required property int count
                            required property real progress

                            width: ListView.view.width
                            height: entryContent.implicitHeight + 16
                            color: Colors.surfaceContainerLowest
                            border.width: 1
                            border.color: Colors.outline
                            clip: true

                            opacity: 0
                            NumberAnimation {
                                id: addAnimation
                                target: entry
                                property: "opacity"
                                from: 0
                                to: 1
                                duration: 180
                                easing.type: Easing.OutCubic
                            }
                            ListView.onAdd: addAnimation.start()
                            // Component.onCompleted also covers rows already
                            // present when the ListView first appears, since
                            // ListView.onAdd only fires for genuine inserts.
                            Component.onCompleted: entry.opacity = 1

                            SequentialAnimation {
                                id: removeAnimation
                                PropertyAction { target: entry; property: "ListView.delayRemove"; value: true }
                                NumberAnimation { target: entry; property: "opacity"; to: 0; duration: 150; easing.type: Easing.InCubic }
                                PropertyAction { target: entry; property: "ListView.delayRemove"; value: false }
                            }
                            // Skipped during a bulk "Clear all" - the
                            // whole-area fade already covers it, and playing
                            // both flickered items back into view mid-fade-in.
                            ListView.onRemove: if (!root.clearingAll) removeAnimation.start()

                            RowLayout {
                                id: entryContent
                                anchors.fill: parent
                                anchors.margins: 8
                                spacing: 8

                                Image {
                                    visible: entry.icon.length > 0
                                    source: entry.icon
                                    Layout.preferredWidth: visible ? 28 : 0
                                    Layout.preferredHeight: 28
                                    Layout.alignment: Qt.AlignVCenter
                                    fillMode: Image.PreserveAspectFit
                                }

                                ColumnLayout {
                                    Layout.fillWidth: true
                                    spacing: 2

                                    RowLayout {
                                        Layout.fillWidth: true

                                        Text {
                                            Layout.fillWidth: true
                                            text: entry.appName
                                            color: Colors.outline
                                            font.family: Style.fontFamily
                                            font.pixelSize: Style.fontSize - 2
                                            elide: Text.ElideRight
                                        }

                                        Text {
                                            text: root.relativeTime(entry.time)
                                            color: Colors.outline
                                            font.family: Style.fontFamily
                                            font.pixelSize: Style.fontSize - 2
                                        }

                                        Rectangle {
                                            Layout.alignment: Qt.AlignVCenter
                                            radius: 0
                                            color: closeMouse.containsMouse ? Colors.surfaceContainerHigh : "transparent"
                                            Behavior on color { ColorAnimation { duration: 120 } }
                                            implicitWidth: 18
                                            implicitHeight: 18

                                            Text {
                                                id: closeLabel
                                                anchors.centerIn: parent
                                                horizontalAlignment: Text.AlignHCenter
                                                verticalAlignment: Text.AlignVCenter
                                                text: "×"
                                                color: Colors.outline
                                                font.family: Style.fontFamily
                                                font.pixelSize: Style.fontSize + 2
                                            }

                                            MouseArea {
                                                id: closeMouse
                                                anchors.fill: parent
                                                hoverEnabled: true
                                                cursorShape: Qt.PointingHandCursor
                                                onClicked: NotificationHistory.dismissEntry(entry.historyId)
                                            }
                                        }
                                    }

                                    Text {
                                        visible: text.length > 0
                                        Layout.fillWidth: true
                                        text: entry.summary + (entry.progress < 0 && entry.count > 1 ? " (" + entry.count + ")" : "")
                                        font.bold: true
                                        color: Colors.onSurfaceVariant
                                        font.family: Style.fontFamily
                                        font.pixelSize: Style.fontSize
                                        wrapMode: Text.Wrap
                                        maximumLineCount: 2
                                        elide: Text.ElideRight
                                    }

                                    Text {
                                        visible: text.length > 0
                                        Layout.fillWidth: true
                                        text: entry.body
                                        textFormat: Text.RichText
                                        color: Colors.onSurfaceVariant
                                        font.family: Style.fontFamily
                                        font.pixelSize: Style.fontSize
                                        wrapMode: Text.Wrap
                                        maximumLineCount: 3
                                        elide: Text.ElideRight
                                    }

                                    RowLayout {
                                        readonly property var actionLabels: NotificationHistory.actionLabelsFor(entry.historyId)
                                        visible: NotificationHistory.isLive(entry.historyId) && actionLabels.length > 0
                                        Layout.topMargin: 10
                                        spacing: 10

                                        Repeater {
                                            model: parent.actionLabels

                                            Rectangle {
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
                                                    text: parent.modelData
                                                    color: Colors.primary
                                                    font.family: Style.fontFamily
                                                    font.pixelSize: Style.fontSize - 2
                                                }

                                                MouseArea {
                                                    id: actionMouse
                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: NotificationHistory.invokeAction(entry.historyId, index)
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
