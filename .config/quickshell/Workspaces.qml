import QtQuick
import QtQuick.Layouts

RowLayout {
    id: root
    spacing: 2

    Repeater {
        model: 9

        Rectangle {
            id: tagButton
            required property int index
            readonly property var tag: Mango.tags[index] ?? ({ index: index + 1, is_active: false, is_urgent: false, client_count: 0 })
            property bool hovered: false

            readonly property bool occupied: tag.client_count > 0
            readonly property bool active: tag.is_active
            readonly property bool urgent: tag.is_urgent

            visible: active || occupied

            Layout.preferredWidth: label.implicitWidth + 8
            Layout.fillHeight: true

            color: urgent ? Colors.error
                 : active ? Colors.onSurfaceVariant
                 : hovered ? Colors.surfaceContainerHigh
                 : occupied ? Colors.surfaceContainerLowest
                 : "transparent"

            Behavior on color { ColorAnimation { duration: 90 } }

            Text {
                id: label
                anchors.centerIn: parent
                text: tagButton.tag.index
                font.family: Style.fontFamily
                font.pixelSize: Style.fontSize
                font.weight: Font.Normal
                color: tagButton.urgent || tagButton.active ? Colors.surfaceContainerLowest
                     : tagButton.occupied ? Colors.onSurfaceVariant
                     : Colors.outline
            }

            MouseArea {
                anchors.fill: parent
                hoverEnabled: true
                onClicked: Mango.switchTag(tagButton.tag.index)
                onEntered: tagButton.hovered = true
                onExited: tagButton.hovered = false
            }
        }
    }
}
