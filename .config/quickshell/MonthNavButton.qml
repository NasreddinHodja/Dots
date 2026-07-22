import QtQuick

Rectangle {
    id: root
    property string glyph: ""
    signal clicked()

    implicitWidth: 24
    implicitHeight: 24
    color: mouse.containsMouse ? Colors.surfaceContainerHigh : "transparent"

    Behavior on color {
        ColorAnimation { duration: 120 }
    }

    Text {
        anchors.centerIn: parent
        text: root.glyph
        color: Colors.onSurfaceVariant
        font.family: Style.fontFamily
        font.pixelSize: Style.fontSize + 2
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
