import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland

Scope {
    id: root

    property int pct: 0
    property bool muted: false
    property string label: ""
    property bool shown: false
    readonly property int maxPct: 150

    IpcHandler {
        target: "osd"

        function volume(pct: int, muted: bool, label: string): void {
            root.pct = Math.max(0, Math.min(root.maxPct, pct))
            root.muted = muted
            root.label = label
            root.shown = true
            hideTimer.restart()
        }
    }

    Timer {
        id: hideTimer
        interval: 1200
        onTriggered: root.shown = false
    }

    PanelWindow {
        visible: root.shown
        WlrLayershell.namespace: "quickshell-osd"
        WlrLayershell.layer: WlrLayer.Overlay

        screen: {
            for (let i = 0; i < Quickshell.screens.length; i++) {
                if (Quickshell.screens[i].name === "DP-3") return Quickshell.screens[i]
            }
            return Quickshell.screens[0]
        }

        anchors {
            right: true
        }

        margins {
            right: 24
        }

        exclusiveZone: 0
        color: "transparent"
        implicitWidth: 56
        implicitHeight: 220

        Rectangle {
            id: frame
            anchors.fill: parent
            color: Colors.surfaceContainerLowest
            radius: 0
            border.width: 3
            border.color: Colors.surfaceContainerLowest

            ColumnLayout {
                anchors.fill: parent
                anchors.margins: 14
                spacing: 16

                Rectangle {
                    id: track
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    color: Colors.surfaceContainerHigh
                    radius: 0

                    Rectangle {
                        id: fill
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.bottom: parent.bottom
                        anchors.margins: 4
                        height: (parent.height - 8) * (root.muted ? 0 : Math.min(root.pct, 100) / 100)
                        radius: 0
                        color: root.muted ? Colors.outline : Colors.primary
                    }

                    Rectangle {
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.bottom: parent.bottom
                        anchors.margins: 4
                        visible: !root.muted && root.pct > 100
                        height: (parent.height - 8) * (Math.max(0, root.pct - 100) / (root.maxPct - 100))
                        radius: 0
                        color: Colors.error
                    }
                }

                Text {
                    Layout.alignment: Qt.AlignHCenter
                    Layout.fillWidth: true
                    text: root.muted || root.pct == 0 ? " " : (root.pct < 50 ? " " : " ")
                    horizontalAlignment: Text.AlignHCenter
                    font.family: Style.fontFamily
                    font.pixelSize: Style.fontSize + 4
                    color: root.muted ? Colors.outline : Colors.onSurfaceVariant
                }

                Text {
                    visible: root.label.length > 0
                    Layout.fillWidth: true
                    text: root.label
                    horizontalAlignment: Text.AlignHCenter
                    font.family: Style.fontFamily
                    font.pixelSize: Style.fontSize - 3
                    color: Colors.outline
                    elide: Text.ElideRight
                    maximumLineCount: 1
                }
            }
        }
    }
}
