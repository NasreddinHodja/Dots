import QtQuick

Text {
    text: "󰒓  " + Mango.layoutSymbol
    color: Colors.onSurfaceVariant
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize

    MouseArea {
        anchors.fill: parent
        onClicked: Mango.cycleLayout()
    }
}
