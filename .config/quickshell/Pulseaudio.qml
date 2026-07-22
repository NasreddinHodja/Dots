import QtQuick
import Quickshell
import Quickshell.Services.Pipewire

Text {
    id: root
    readonly property var sink: Pipewire.defaultAudioSink
    readonly property real volume: sink?.audio?.volume ?? 0
    readonly property bool muted: sink?.audio?.muted ?? false

    readonly property var props: sink?.properties ?? ({})
    readonly property string formFactor: props["device.form-factor"] ?? ""
    readonly property bool isBluetooth: props["device.api"] === "bluez5" || props["api.bluez5.address"] !== undefined

    function formFactorIcon() {
        switch (formFactor) {
        case "headphone": case "headset": return ""
        case "hands-free": return ""
        case "phone": case "portable": return ""
        case "car": return ""
        default: return ""
        }
    }

    PwObjectTracker {
        objects: [Pipewire.defaultAudioSink]
    }

    readonly property string baseIcon: muted
        ? ""
        : (formFactorIcon().length > 0 ? formFactorIcon() : (volume < 0.5 ? " " : " "))
    readonly property string icon: baseIcon + (isBluetooth ? "" : "")

    text: icon + " " + Math.round(volume * 100) + "%"
    color: muted ? Colors.outline : Colors.onSurfaceVariant
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.MiddleButton
        onClicked: (mouse) => {
            if (mouse.button === Qt.MiddleButton) {
                if (root.sink?.audio) root.sink.audio.muted = !root.sink.audio.muted
            } else {
                Quickshell.execDetached(["pavucontrol"])
            }
        }
        onWheel: (wheel) => {
            if (!root.sink?.audio) return
            const step = 0.05
            const next = root.sink.audio.volume + (wheel.angleDelta.y > 0 ? step : -step)
            root.sink.audio.volume = Math.max(0, Math.min(1, next))
        }
    }
}
