import QtQuick
import Quickshell
import Quickshell.Io

Text {
    id: root
    property string rawText: ""

    textFormat: Text.RichText
    text: rawText.replace(/fgcolor='([^']+)'/g, "style='color:$1'")
    color: Colors.onSurfaceVariant
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize

    Process {
        id: sysmonProc
        command: ["bash", "-c", "~/.local/bin/hypr-sysmon"]

        stdout: StdioCollector {
            onStreamFinished: {
                try {
                    root.rawText = JSON.parse(this.text).text
                } catch (e) {
                    console.warn("SysMon: failed to parse output")
                }
            }
        }
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: sysmonProc.running = true
    }

    MouseArea {
        anchors.fill: parent
        onClicked: Quickshell.execDetached(["missioncenter"])
    }
}
