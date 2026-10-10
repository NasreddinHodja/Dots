import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Mpris

Item {
    id: root
    readonly property var player: Mpris.players.values.length > 0 ? Mpris.players.values[0] : null
    readonly property string title: player?.trackTitle ?? ""
    readonly property string artist: player?.trackArtist ?? ""
    readonly property string full: artist.length > 0 ? artist + " - " + title : title
    readonly property int textBoxWidth: 100
    readonly property int gap: 30
    readonly property real scrollSpeed: 30

    function playerIcon() {
        switch (player?.identity) {
        case "Spotify": return "󰓇 "
        case "mpv": return " "
        case "Firefox": case "Zen Browser": return "󰈹 "
        case "VLC": return "󰕼 "
        default: return "󰝚 "
        }
    }

    readonly property string icons: player ? (player.playbackState === MprisPlaybackState.Playing ? "󰏤 " : "󰐊 ") + playerIcon() : ""
    readonly property bool overflowing: label.implicitWidth > root.textBoxWidth

    visible: player !== null && title.length > 0
    implicitWidth: iconLabel.implicitWidth + root.textBoxWidth
    implicitHeight: iconLabel.implicitHeight
    Layout.preferredWidth: implicitWidth
    Layout.minimumWidth: implicitWidth
    Layout.maximumWidth: implicitWidth

    onFullChanged: {
        marquee.x = 0
        scrollAnim.restart()
    }
    onOverflowingChanged: if (!overflowing) marquee.x = 0

    RowLayout {
        anchors.fill: parent
        spacing: 0

        Text {
            id: iconLabel
            text: root.icons
            font.italic: true
            color: Colors.onSurfaceVariant
            font.family: Style.fontFamily
            font.pixelSize: Style.fontSize
        }

        Item {
            Layout.preferredWidth: root.textBoxWidth
            Layout.minimumWidth: root.textBoxWidth
            Layout.maximumWidth: root.textBoxWidth
            Layout.fillHeight: true
            clip: true

            Row {
                id: marquee
                anchors.verticalCenter: parent.verticalCenter
                spacing: root.gap

                Text {
                    id: label
                    font.italic: true
                    text: root.full
                    color: Colors.onSurfaceVariant
                    font.family: Style.fontFamily
                    font.pixelSize: Style.fontSize
                }

                Text {
                    visible: root.overflowing
                    font.italic: true
                    text: root.full
                    color: Colors.onSurfaceVariant
                    font.family: Style.fontFamily
                    font.pixelSize: Style.fontSize
                }
            }

            NumberAnimation {
                id: scrollAnim
                target: marquee
                property: "x"
                from: 0
                to: -(label.implicitWidth + root.gap)
                duration: (label.implicitWidth + root.gap) / root.scrollSpeed * 1000
                loops: Animation.Infinite
                running: root.overflowing && root.visible
            }
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.player?.togglePlaying()
    }
}
