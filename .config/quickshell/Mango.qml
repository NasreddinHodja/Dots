pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    readonly property string monitorName: "DP-3"
    readonly property string secondMonitorName: "HDMI-A-1"

    property var monitorData: ({})
    property var allMonitorTags: ({})

    readonly property var tags: monitorData.tags ?? []
    readonly property string layoutSymbol: monitorData.layout_symbol ?? ""
    readonly property string activeTitle: monitorData.active_client?.title ?? ""
    readonly property string activeAppId: monitorData.active_client?.appid ?? ""

    readonly property var otherActiveTagIndices: {
        let result = []
        for (const name in allMonitorTags) {
            if (name === monitorName) continue
            for (const t of allMonitorTags[name]) {
                if (t.is_active) result.push(t.index)
            }
        }
        return result
    }

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
        id: allTagsWatch
        command: ["mmsg", "watch", "all-tags"]
        running: true

        stdout: SplitParser {
            onRead: (line) => {
                try {
                    const data = JSON.parse(line)
                    let map = {}
                    for (const m of data.all_tags) map[m.monitor] = m.tags
                    allMonitorTags = map
                } catch (e) {
                    console.warn("Mango: failed to parse mmsg all-tags output:", line)
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
