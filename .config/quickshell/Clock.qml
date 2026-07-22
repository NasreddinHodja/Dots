import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland

Text {
    id: root
    property bool detailOpen: false

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
        anchors.fill: parent
        onClicked: root.detailOpen = !root.detailOpen
    }

    // Reopen on the current month instead of a stale prev/next position.
    onDetailOpenChanged: if (detailOpen) { viewedDate = new Date(); card.resetToToday() }

    // Full-screen overlay (not edge-anchored) so clicking anywhere outside
    // the card can dismiss it; the card itself is positioned manually.
    PanelWindow {
        id: overlay
        WlrLayershell.namespace: "quickshell-clock-detail"
        WlrLayershell.layer: WlrLayer.Overlay

        screen: {
            for (let i = 0; i < Quickshell.screens.length; i++) {
                if (Quickshell.screens[i].name === "DP-3") return Quickshell.screens[i]
            }
            return Quickshell.screens[0]
        }

        anchors { top: true; bottom: true; left: true; right: true }
        exclusionMode: ExclusionMode.Ignore
        color: "transparent"

        visible: root.detailOpen

        MouseArea {
            anchors.fill: parent
            onClicked: root.detailOpen = false
        }

        Rectangle {
            id: card
            // Bar.qml insets the bar 200px + an 8px row margin, so the
            // clock's right edge sits 208px from the screen edge; the bar
            // itself is 30px tall (24px + 6px margin), hence the y offset.
            readonly property int padding: 12

            x: overlay.width - 208 - width
            y: overlay.height - 32 - height
            width: calendar.implicitWidth + padding * 2
            height: calendar.implicitHeight + padding * 2
            color: Colors.surfaceContainerLowest
            border.width: 1
            border.color: Colors.outline

            // Swallows clicks so they don't fall through to the dismiss
            // MouseArea behind the card.
            MouseArea {
                anchors.fill: parent
            }

            readonly property int cellSize: 20
            readonly property int gridSpacing: 4
            readonly property int gridWidth: cellSize * 7 + gridSpacing * 6
            readonly property int gridHeight: cellSize * 6 + gridSpacing * 5
            readonly property var weekdayLabels: ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]
            readonly property date realNow: systemClock.date

            property bool activeIsA: true
            property date dateA: new Date()
            property date dateB: new Date()

            function resetToToday() {
                slideAnim.stop()
                activeIsA = true
                dateA = new Date()
                gridA.x = 0
                gridB.x = card.gridWidth
            }

            // root.viewedDate always holds the last-requested month, so
            // rapid clicks keep advancing even if the previous slide is
            // still animating (or gets cut off by restart() below).
            function shiftMonth(delta) {
                const newDate = new Date(root.viewedDate.getFullYear(), root.viewedDate.getMonth() + delta, 1)
                root.viewedDate = newDate

                const w = card.gridWidth
                const enteringA = !card.activeIsA
                if (enteringA) {
                    card.dateA = newDate
                    gridA.x = delta > 0 ? w : -w
                    outAnim.target = gridB
                    inAnim.target = gridA
                } else {
                    card.dateB = newDate
                    gridB.x = delta > 0 ? w : -w
                    outAnim.target = gridA
                    inAnim.target = gridB
                }
                outAnim.to = delta > 0 ? -w : w
                inAnim.to = 0
                card.activeIsA = enteringA
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
                anchors.margins: card.padding
                spacing: 6

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 4

                    MonthNavButton {
                        glyph: "‹"
                        onClicked: card.shiftMonth(-1)
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
                        onClicked: card.shiftMonth(1)
                    }
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: card.gridSpacing

                    Repeater {
                        model: card.weekdayLabels
                        Text {
                            Layout.preferredWidth: card.cellSize
                            Layout.preferredHeight: card.cellSize
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
                    Layout.preferredWidth: card.gridWidth
                    Layout.preferredHeight: card.gridHeight
                    clip: true

                    MonthGrid {
                        id: gridA
                        cellSize: card.cellSize
                        columnSpacing: card.gridSpacing
                        rowSpacing: card.gridSpacing
                        monthDate: card.dateA
                        today: card.realNow
                    }

                    MonthGrid {
                        id: gridB
                        cellSize: card.cellSize
                        columnSpacing: card.gridSpacing
                        rowSpacing: card.gridSpacing
                        monthDate: card.dateB
                        today: card.realNow
                        x: card.gridWidth
                    }
                }
            }
        }
    }
}
