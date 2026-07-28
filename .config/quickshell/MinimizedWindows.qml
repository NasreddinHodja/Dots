import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets

RowLayout {
    id: root
    spacing: 4

    readonly property var minimizedClients: Mango.minimizedClients

    function iconFor(appid) {
        const entry = DesktopEntries.byId(appid) ?? DesktopEntries.heuristicLookup(appid)
        return entry ? Quickshell.iconPath(entry.icon, true) : ""
    }

    Repeater {
        model: root.minimizedClients

        Rectangle {
            id: chip
            required property var modelData

            implicitWidth: 20
            implicitHeight: 20
            radius: 0
            color: mouse.containsMouse ? Colors.surfaceContainerHigh : "transparent"
            Behavior on color { ColorAnimation { duration: 120 } }

            IconImage {
                anchors.centerIn: parent
                implicitSize: 14
                source: root.iconFor(chip.modelData.appid)
            }

            MouseArea {
                id: mouse
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: Mango.dispatch(`restore_minimized,1 client,${chip.modelData.id}`)
            }
        }
    }
}
