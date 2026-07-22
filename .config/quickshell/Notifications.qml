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
            // Mimic dunst's stacking: a notification sharing the same app + tag
            // (x-dunst-stack-tag, or the GNOME-style x-canonical-private-synchronous
            // used by volume/brightness OSD scripts), or the same app + summary + body
            // (plain duplicate), replaces the previous bubble instead of piling up a
            // new one, and bumps a "(N)" counter instead. This also covers scripts that
            // rely on a hardcoded --replace-id, since a client-picked id never matches
            // a real server-assigned notification id and so never actually replaces.
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
                existing.dismiss()
            }

            notification.tracked = true
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
        implicitWidth: 300
        implicitHeight: column.implicitHeight

        ColumnLayout {
            id: column
            width: parent.width
            spacing: 12

            Repeater {
                model: server.trackedNotifications

                Rectangle {
                    id: bubble
                    required property Notification modelData

                    // Per spec, expireTimeout is -1 when the app leaves it up to the server,
                    // 0 when the app asks to never expire, and >0 for an explicit duration.
                    // Only the -1 case falls back to dunstrc's per-urgency default
                    // (urgency_low/normal = 10s, urgency_critical = never).
                    function timeoutMs() {
                        const t = bubble.modelData.expireTimeout
                        if (t >= 0) return t
                        return bubble.modelData.urgency === NotificationUrgency.Critical ? 0 : 10000
                    }

                    function dupCount() {
                        return root.dupCounts[bubble.modelData.id] || 1
                    }

                    function progressValue() {
                        const v = bubble.modelData.hints["value"]
                        if (v === undefined || v === "") return -1
                        const n = Number(v)
                        if (isNaN(n)) return -1
                        return Math.max(0, Math.min(100, n))
                    }

                    Layout.fillWidth: true
                    implicitHeight: content.implicitHeight + 32

                    color: Colors.surfaceContainerLowest
                    radius: 0
                    border.width: 3
                    border.color: bubble.modelData.urgency === NotificationUrgency.Critical
                        ? Colors.error
                        : Colors.surfaceContainerLowest

                    RowLayout {
                        id: content
                        anchors.fill: parent
                        anchors.margins: 16
                        spacing: 12

                        Image {
                            readonly property string resolved: {
                                // Read the raw image-path hint ourselves instead of trusting
                                // modelData.image: Quickshell's own hint processing wraps an
                                // absolute path into a broken theme-icon lookup instead of
                                // loading it as a file.
                                const rawPath = bubble.modelData.hints["image-path"] || bubble.modelData.hints["image_path"] || ""
                                const icon = bubble.modelData.appIcon || rawPath || bubble.modelData.image
                                if (!icon) return ""
                                if (icon.startsWith("/")) return "file://" + icon
                                if (icon.startsWith("file://") || icon.startsWith("http") || icon.startsWith("image://")) return icon
                                return Quickshell.iconPath(icon)
                            }
                            visible: resolved.length > 0
                            Layout.preferredWidth: visible ? 32 : 0
                            Layout.preferredHeight: 32
                            Layout.alignment: Qt.AlignVCenter
                            source: resolved
                            fillMode: Image.PreserveAspectFit
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 4

                            Text {
                                visible: text.length > 0
                                Layout.fillWidth: true
                                text: bubble.modelData.appName
                                font.family: Style.fontFamily
                                font.pixelSize: Style.fontSize - 2
                                color: Colors.outline
                                wrapMode: Text.Wrap
                                elide: Text.ElideRight
                                maximumLineCount: 1
                            }

                            Text {
                                Layout.fillWidth: true
                                text: bubble.modelData.summary + (bubble.progressValue() < 0 && bubble.dupCount() > 1 ? " (" + bubble.dupCount() + ")" : "")
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
                                text: bubble.modelData.body
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
                                visible: bubble.progressValue() >= 0
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
                                    width: parent.width * (bubble.progressValue() / 100)
                                }
                            }
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        acceptedButtons: Qt.LeftButton | Qt.MiddleButton | Qt.RightButton
                        onClicked: (mouse) => {
                            if (mouse.button === Qt.LeftButton) {
                                bubble.modelData.dismiss()
                            } else if (mouse.button === Qt.MiddleButton) {
                                if (bubble.modelData.actions.length > 0) bubble.modelData.actions[0].invoke()
                                bubble.modelData.dismiss()
                            } else if (mouse.button === Qt.RightButton) {
                                for (const n of [...server.trackedNotifications.values]) n.dismiss()
                            }
                        }
                    }

                    Timer {
                        running: bubble.timeoutMs() > 0
                        interval: bubble.timeoutMs()
                        onTriggered: bubble.modelData.expire()
                    }
                }
            }
        }
    }
}
