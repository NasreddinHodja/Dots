import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import Quickshell.Services.SystemTray

RowLayout {
    id: root
    required property PanelWindow barWindow
    spacing: 6

    Repeater {
        model: SystemTray.items

        Item {
            id: trayIcon
            required property SystemTrayItem modelData

            Layout.preferredWidth: 14
            Layout.preferredHeight: 14
            Layout.alignment: Qt.AlignVCenter

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
}
