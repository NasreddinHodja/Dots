import QtQuick
import QtQuick.Layouts

Text {
    readonly property string title: Mango.activeTitle
    readonly property int maxWidth: 300

    visible: title.length > 0
    Layout.maximumWidth: maxWidth
    width: Math.min(implicitWidth, maxWidth)
    clip: true
    elide: Text.ElideRight

    text: title.length > 0 ? "󰘔  " + title : ""
    color: Colors.onSurfaceVariant
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize
}
