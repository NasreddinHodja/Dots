import QtQuick
import QtQuick.Layouts

RowLayout {
    id: root
    spacing: 2

    required property var tags
    property string monitorName: Mango.monitorName
    // tag indices considered "active" on some other monitor, underlined here
    property var otherActiveTagIndices: []

    Repeater {
        model: 9

        Rectangle {
            id: tagButton
            required property int index
            readonly property var tag: root.tags[index] ?? ({ index: index + 1, is_active: false, is_urgent: false, client_count: 0 })
            property bool hovered: false

            readonly property bool occupied: tag.client_count > 0
            readonly property bool active: tag.is_active
            readonly property bool urgent: tag.is_urgent
            readonly property bool activeElsewhere: !active && root.otherActiveTagIndices.includes(tag.index)

            visible: active || occupied || activeElsewhere

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
                     : tagButton.occupied || tagButton.activeElsewhere ? Colors.onSurfaceVariant
                     : Colors.outline
            }

            Rectangle {
                visible: tagButton.activeElsewhere
                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter
                width: label.implicitWidth
                height: 1
                color: Colors.onSurfaceVariant
            }

            MouseArea {
                anchors.fill: parent
                hoverEnabled: true
                onClicked: Mango.dispatch(`viewcrossmon,${tagButton.tag.index},${root.monitorName}`)
                onEntered: tagButton.hovered = true
                onExited: tagButton.hovered = false
            }
        }
    }
}
