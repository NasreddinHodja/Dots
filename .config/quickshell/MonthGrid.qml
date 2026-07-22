import QtQuick
import QtQuick.Layouts

// Day-number cells only (the static weekday header lives once in Clock.qml)
// so two instances can slide past each other for the month-change carousel.
GridLayout {
    id: grid
    required property date monthDate
    required property date today
    property int cellSize: 20

    columns: 7
    rowSpacing: 4
    columnSpacing: 4

    readonly property int year: monthDate.getFullYear()
    readonly property int month: monthDate.getMonth()
    readonly property int firstWeekday: new Date(year, month, 1).getDay()
    readonly property int daysInMonth: new Date(year, month + 1, 0).getDate()
    readonly property bool isCurrentMonth: year === today.getFullYear() && month === today.getMonth()

    function dayAt(cellIndex) {
        const dayNum = cellIndex - grid.firstWeekday + 1
        return (dayNum < 1 || dayNum > grid.daysInMonth) ? 0 : dayNum
    }

    Repeater {
        model: 42
        Rectangle {
            required property int index
            readonly property int dayNum: grid.dayAt(index)
            readonly property bool isToday: grid.isCurrentMonth && dayNum === grid.today.getDate()

            Layout.preferredWidth: grid.cellSize
            Layout.preferredHeight: grid.cellSize
            color: isToday ? Colors.primary : "transparent"

            Text {
                anchors.centerIn: parent
                visible: parent.dayNum > 0
                text: parent.dayNum
                color: parent.isToday ? Colors.surfaceContainerLowest : Colors.onSurfaceVariant
                font.family: Style.fontFamily
                font.pixelSize: Style.fontSize
            }
        }
    }
}
