import QtQuick
import QtQuick.Layouts
import Quickshell

Text {
    id: root
    property bool detailOpen: false
    property var barWindow: null

    color: Colors.onSurfaceVariant
    text: "  " + Qt.formatDateTime(systemClock.date, "ddd, dd/MM/yyyy HH:mm")
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize

    property date viewedDate: new Date()

    SystemClock {
        id: systemClock
        precision: SystemClock.Minutes
    }

    MouseArea {
        id: clockMouse
        anchors.fill: parent
        anchors.margins: -6
        onClicked: root.detailOpen = !root.detailOpen
    }

    // viewedDate outlives the card (which is recreated fresh by the Loader
    // below each open), so it needs resetting here to avoid a stale month.
    onDetailOpenChanged: if (detailOpen) viewedDate = new Date()

    Loader {
        active: root.detailOpen
        sourceComponent: overlayComponent
    }

    Component {
        id: overlayComponent

        PopupPanel {
            id: popup
            namespace: "quickshell-clock-detail"
            anchorItem: root
            barWindow: root.barWindow
            cardWidth: calendar.implicitWidth + padding * 2
            cardHeight: calendar.implicitHeight + padding * 2
            onDismissed: root.detailOpen = false

            readonly property int cellSize: 20
            readonly property int gridSpacing: 4
            readonly property int gridWidth: cellSize * 7 + gridSpacing * 6
            readonly property int gridHeight: cellSize * 6 + gridSpacing * 5
            readonly property var weekdayLabels: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]
            readonly property date realNow: systemClock.date

            property bool activeIsA: true
            property date dateA: new Date()
            property date dateB: new Date()

            // Reads root.viewedDate, not gridA/B's dates, so rapid clicks
            // keep advancing even if the previous slide is still animating.
            function shiftMonth(delta) {
                const newDate = new Date(root.viewedDate.getFullYear(), root.viewedDate.getMonth() + delta, 1)
                root.viewedDate = newDate

                const w = popup.gridWidth
                const enteringA = !popup.activeIsA
                if (enteringA) {
                    popup.dateA = newDate
                    gridA.x = delta > 0 ? w : -w
                    outAnim.target = gridB
                    inAnim.target = gridA
                } else {
                    popup.dateB = newDate
                    gridB.x = delta > 0 ? w : -w
                    outAnim.target = gridA
                    inAnim.target = gridB
                }
                outAnim.to = delta > 0 ? -w : w
                inAnim.to = 0
                popup.activeIsA = enteringA
                slideAnim.restart()
            }

            ParallelAnimation {
                id: slideAnim
                NumberAnimation { id: outAnim; property: "x"; duration: 220; easing.type: Easing.OutCubic }
                NumberAnimation { id: inAnim; property: "x"; duration: 220; easing.type: Easing.OutCubic }
            }

            ColumnLayout {
                id: calendar
                anchors.fill: parent
                spacing: 6

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 4

                    MonthNavButton {
                        glyph: "‹"
                        onClicked: popup.shiftMonth(-1)
                    }

                    Text {
                        Layout.fillWidth: true
                        horizontalAlignment: Text.AlignHCenter
                        text: Qt.formatDate(root.viewedDate, "MMMM yyyy")
                        color: Colors.onSurfaceVariant
                        font.family: Style.fontFamily
                        font.pixelSize: Style.fontSize
                        font.bold: true
                    }

                    MonthNavButton {
                        glyph: "›"
                        onClicked: popup.shiftMonth(1)
                    }
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: popup.gridSpacing

                    Repeater {
                        model: popup.weekdayLabels
                        Text {
                            Layout.preferredWidth: popup.cellSize
                            Layout.preferredHeight: popup.cellSize
                            horizontalAlignment: Text.AlignHCenter
                            verticalAlignment: Text.AlignVCenter
                            text: modelData
                            color: Colors.outline
                            font.family: Style.fontFamily
                            font.pixelSize: Style.fontSize
                        }
                    }
                }

                Item {
                    id: gridViewport
                    Layout.preferredWidth: popup.gridWidth
                    Layout.preferredHeight: popup.gridHeight
                    clip: true

                    MonthGrid {
                        id: gridA
                        cellSize: popup.cellSize
                        columnSpacing: popup.gridSpacing
                        rowSpacing: popup.gridSpacing
                        monthDate: popup.dateA
                        today: popup.realNow
                    }

                    MonthGrid {
                        id: gridB
                        cellSize: popup.cellSize
                        columnSpacing: popup.gridSpacing
                        rowSpacing: popup.gridSpacing
                        monthDate: popup.dateB
                        today: popup.realNow
                        x: popup.gridWidth
                    }
                }
            }
        }
    }
}
