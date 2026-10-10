import QtQuick
import Quickshell.Io

Text {
    id: root
    property bool capsLocked: false

    text: capsLocked ? "⟪▲⟫" : "⟪△⟫"
    color: capsLocked ? Colors.onSurfaceVariant : Colors.outline
    font.family: Style.fontFamily
    font.pixelSize: Style.fontSize

    // Note: sysfs LED brightness notifies via poll()/epoll, not inotify, so
    // FileView's watchChanges (inotify-based) never fires here. Poll instead.
    FileView {
        id: capslockFile
        onLoaded: root.capsLocked = text().trim() === "1"
    }

    Timer {
        interval: 250
        running: capslockFile.path.length > 0
        repeat: true
        onTriggered: capslockFile.reload()
    }

    Process {
        id: findLed
        command: ["sh", "-c", "ls -d /sys/class/leds/*capslock* 2>/dev/null | head -1"]
        running: true

        stdout: StdioCollector {
            onStreamFinished: {
                const path = this.text.trim()
                if (path.length > 0) capslockFile.path = path + "/brightness"
            }
        }
    }
}
