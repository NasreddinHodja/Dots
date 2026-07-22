import QtQuick
import Quickshell

Text {
    color: Colors.onSurfaceVariant
    text: Qt.formatDateTime(systemClock.date, "ddd, MMM dd HH:mm")
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize

    SystemClock {
        id: systemClock
        precision: SystemClock.Minutes
    }
}
