import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import Quickshell.Services.SystemTray

Item {
    id: root
    required property PanelWindow barWindow
    // Beyond this many, icons collapse into the overflow popup instead of
    // growing the bar - keeps a noisy tray (Steam, Discord, printers, etc.)
    // from pushing everything else around.
    property int maxVisible: 3

    readonly property var trayItems: SystemTray.items.values
    readonly property var pinnedItems: root.trayItems.slice(0, root.maxVisible)
    readonly property var overflowItems: root.trayItems.slice(root.maxVisible)
    property bool overflowOpen: false

    implicitWidth: rowLayout.implicitWidth
    implicitHeight: rowLayout.implicitHeight

    Component {
        id: iconDelegate

        Item {
            id: trayIcon
            required property SystemTrayItem modelData

            implicitWidth: 14
            implicitHeight: 14

            IconImage {
                anchors.fill: parent
                source: trayIcon.modelData.icon
                implicitSize: 14
            }

            QsMenuAnchor {
                id: menuAnchor
                menu: trayIcon.modelData.menu
                anchor.window: root.barWindow
            }

            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.LeftButton | Qt.RightButton
                onClicked: (mouse) => {
                    if (mouse.button === Qt.RightButton && trayIcon.modelData.hasMenu) {
                        menuAnchor.open()
                    } else {
                        trayIcon.modelData.activate()
                    }
                }
            }
        }
    }

    RowLayout {
        id: rowLayout
        anchors.fill: parent
        spacing: 6

        Text {
            id: overflowIcon
            visible: root.overflowItems.length > 0
            Layout.alignment: Qt.AlignVCenter
            Layout.rightMargin: visible ? 3 : 0
            text: root.overflowOpen ? "󰅀" : "󰅃"
            color: root.overflowOpen ? Colors.primary : Colors.onSurfaceVariant
            font.family: Style.fontFamily
            font.pixelSize: Style.fontSize

            MouseArea {
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: root.overflowOpen = !root.overflowOpen
            }
        }

        Repeater {
            model: root.pinnedItems
            delegate: iconDelegate
        }
    }

    Loader {
        active: root.overflowOpen
        sourceComponent: overlayComponent
    }

    Component {
        id: overlayComponent

        PopupPanel {
            namespace: "quickshell-tray-overflow"
            anchorItem: overflowIcon
            barWindow: root.barWindow
            cardWidth: overflowRow.implicitWidth + 2 * padding
            cardHeight: 14 + 2 * padding
            onDismissed: root.overflowOpen = false

            RowLayout {
                id: overflowRow
                anchors.fill: parent
                spacing: 6

                Repeater {
                    model: root.overflowItems
                    delegate: iconDelegate
                }
            }
        }
    }
}
