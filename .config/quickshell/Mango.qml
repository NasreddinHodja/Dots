pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    readonly property string monitorName: "DP-3"

    property var monitorData: ({})

    readonly property var tags: monitorData.tags ?? []
    readonly property string layoutSymbol: monitorData.layout_symbol ?? ""
    readonly property string activeTitle: monitorData.active_client?.title ?? ""
    readonly property string activeAppId: monitorData.active_client?.appid ?? ""

    function dispatch(cmd) {
        dispatchProc.command = ["mmsg", "dispatch", cmd]
        dispatchProc.running = true
    }

    function switchTag(index) {
        dispatch(`viewcrossmon,${index},${monitorName}`)
    }

    function cycleLayout() {
        dispatch("switch_layout")
    }

    Process {
        id: monitorWatch
        command: ["mmsg", "watch", "monitor", monitorName]
        running: true

        stdout: SplitParser {
            onRead: (line) => {
                try {
                    monitorData = JSON.parse(line)
                } catch (e) {
                    console.warn("Mango: failed to parse mmsg output:", line)
                }
            }
        }

        onRunningChanged: {
            if (!running) running = true
        }
    }

    Process {
        id: dispatchProc
    }
}
