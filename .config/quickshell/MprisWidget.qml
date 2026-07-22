import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Mpris

Text {
    id: root
    readonly property var player: Mpris.players.values.length > 0 ? Mpris.players.values[0] : null
    readonly property string title: player?.trackTitle ?? ""
    readonly property string artist: player?.trackArtist ?? ""
    readonly property string full: artist.length > 0 ? artist + " - " + title : title
    readonly property int maxWidth: 300

    function playerIcon() {
        switch (player?.identity) {
        case "Spotify": return "󰓇 "
        case "mpv": return " "
        case "Firefox": case "Zen Browser": return "󰈹 "
        case "VLC": return "󰕼 "
        default: return "󰝚 "
        }
    }

    visible: player !== null && title.length > 0
    Layout.maximumWidth: maxWidth
    width: Math.min(implicitWidth, maxWidth)
    clip: true
    elide: Text.ElideRight
    font.italic: true
    text: player ? (player.playbackState === MprisPlaybackState.Playing ? "󰏤 " : "󰐊 ") + playerIcon() + full : ""
    color: Colors.onSurfaceVariant
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize

    MouseArea {
        anchors.fill: parent
        onClicked: root.player?.togglePlaying()
    }
}
