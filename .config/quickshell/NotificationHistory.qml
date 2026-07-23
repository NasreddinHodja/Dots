pragma Singleton
import QtQuick
import QtQml.Models
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    // Do Not Disturb: while true, incoming notifications are still logged to
    // history but never shown as a bubble. Resets to false on shell restart,
    // matching dunst's own set-paused behavior.
    property bool dndEnabled: false

    readonly property int maxEntries: 200
    property int unreadCount: 0

    // A real ListModel, not a plain array: reassigning an array wholesale
    // looks like a full reset to QtQuick, so ListView's add/remove/displaced
    // transitions never fire on it.
    property alias historyModel: _historyModel
    ListModel { id: _historyModel }

    property int _nextId: 1
    // historyId -> live Notification, only while still open server-side
    // (destroyed the instant it closes). Lets the panel dismiss/invoke
    // actions on entries still showing as a bubble; entries restored from
    // disk or already closed fall back to read-only.
    property var _liveRefs: ({})

    function isLive(historyId) {
        return root._liveRefs[historyId] !== undefined
    }

    // Some notify-send builds don't parse "-A identifier,label" at all:
    // they auto-number the real identifier and pass "identifier,label"
    // through as the label verbatim. Detect that shape directly (a short
    // token immediately followed by a comma with no space - real labels
    // like "Reply, ignore" have a space after the comma) since comparing
    // against action.identifier doesn't help there. Public so the bubbles
    // in Notifications.qml can reuse it too.
    function cleanActionLabel(action) {
        if (!action) return ""
        const match = action.text.match(/^[\w-]+,(?=\S)/)
        return match ? action.text.slice(match[0].length) : action.text
    }

    function actionLabelsFor(historyId) {
        const n = root._liveRefs[historyId]
        return n ? n.actions.map(a => root.cleanActionLabel(a)) : []
    }

    function resolveIcon(notification) {
        const rawPath = notification.hints["image-path"] || notification.hints["image_path"] || ""
        const icon = notification.appIcon || rawPath || notification.image
        if (!icon) return ""
        if (icon.startsWith("/")) return "file://" + icon
        if (icon.startsWith("file://") || icon.startsWith("http") || icon.startsWith("image://")) return icon
        return Quickshell.iconPath(icon)
    }

    // OSD-style notifications (volume, brightness) carry a numeric "value"
    // hint and shouldn't show a "(N)" stack count instead.
    function progressValue(notification) {
        const v = notification.hints["value"]
        if (v === undefined || v === "") return -1
        const n = Number(v)
        if (isNaN(n)) return -1
        return Math.max(0, Math.min(100, n))
    }

    // Same stacking rule as the bubbles: same app + tag, or same app +
    // summary + body, refreshes the existing row instead of spamming a new
    // one per volume-scroll tick.
    function _findStackIndex(notification, tag) {
        for (let i = 0; i < _historyModel.count; i++) {
            const row = _historyModel.get(i)
            if (row.appName !== notification.appName) continue
            const isMatch = tag
                ? row.tag === tag
                : (row.summary === notification.summary && row.body === notification.body)
            if (isMatch) return i
        }
        return -1
    }

    function _linkLive(id, notification) {
        root._liveRefs[id] = notification
        // Guard against a stale bubble's closed() firing after this id has
        // already been re-linked to a newer stacked notification.
        notification.closed.connect(() => {
            if (root._liveRefs[id] === notification) delete root._liveRefs[id]
        })
    }

    function record(notification) {
        const tag = notification.hints["x-dunst-stack-tag"] || notification.hints["x-canonical-private-synchronous"] || ""
        const stackIndex = root._findStackIndex(notification, tag)

        if (stackIndex >= 0) {
            const old = _historyModel.get(stackIndex)
            const id = old.historyId
            _historyModel.set(stackIndex, {
                historyId: id,
                time: Date.now(),
                appName: notification.appName,
                icon: root.resolveIcon(notification),
                summary: notification.summary,
                body: notification.body,
                urgency: notification.urgency,
                read: false,
                tag: tag,
                count: (old.count || 1) + 1,
                progress: root.progressValue(notification),
            })
            root._linkLive(id, notification)
            root._persist()
            return
        }

        const id = root._nextId++
        _historyModel.insert(0, {
            historyId: id,
            time: Date.now(),
            appName: notification.appName,
            icon: root.resolveIcon(notification),
            summary: notification.summary,
            body: notification.body,
            urgency: notification.urgency,
            read: false,
            tag: tag,
            count: 1,
            progress: root.progressValue(notification),
        })
        if (_historyModel.count > root.maxEntries) _historyModel.remove(_historyModel.count - 1)

        root._linkLive(id, notification)

        root.unreadCount++
        root._persist()
    }

    function invokeAction(historyId, index) {
        const n = root._liveRefs[historyId]
        if (n && n.actions[index]) n.actions[index].invoke()
        root.dismissEntry(historyId)
    }

    function _indexOf(historyId) {
        for (let i = 0; i < _historyModel.count; i++) {
            if (_historyModel.get(i).historyId === historyId) return i
        }
        return -1
    }

    function dismissEntry(historyId) {
        const n = root._liveRefs[historyId]
        if (n) n.dismiss()
        delete root._liveRefs[historyId]
        const idx = root._indexOf(historyId)
        if (idx >= 0) _historyModel.remove(idx)
        root._persist()
    }

    function markAllRead() {
        if (root.unreadCount === 0) return
        for (let i = 0; i < _historyModel.count; i++) {
            if (!_historyModel.get(i).read) _historyModel.setProperty(i, "read", true)
        }
        root.unreadCount = 0
        root._persist()
    }

    function clearAll() {
        // _liveRefs already holds exactly the currently-tracked bubbles, so
        // this closes them too without needing NotificationServer directly.
        for (const n of Object.values(root._liveRefs)) n.dismiss()
        root._liveRefs = {}

        _historyModel.clear()
        root.unreadCount = 0
        root._persist()
    }

    function toggleDnd() {
        root.dndEnabled = !root.dndEnabled
    }

    // Debounced: record() can fire many times a second during a volume/
    // brightness OSD scroll, and each call would otherwise rewrite the
    // whole history file to disk immediately.
    Timer {
        id: persistTimer
        interval: 300
        onTriggered: root._writeToDisk()
    }

    function _persist() {
        persistTimer.restart()
    }

    function _writeToDisk() {
        const entries = []
        for (let i = 0; i < _historyModel.count; i++) {
            const e = _historyModel.get(i)
            entries.push({
                historyId: e.historyId,
                time: e.time,
                appName: e.appName,
                icon: e.icon,
                summary: e.summary,
                body: e.body,
                urgency: e.urgency,
                read: e.read,
                tag: e.tag,
                count: e.count,
                progress: e.progress,
            })
        }
        historyAdapter.entries = entries
        historyAdapter.nextId = root._nextId
        historyFile.writeAdapter()
    }

    FileView {
        id: historyFile
        path: "/home/nasreddin/.config/quickshell/notification_history.json"

        adapter: JsonAdapter {
            id: historyAdapter
            property list<var> entries: []
            property int nextId: 1
        }

        onLoaded: {
            const entries = historyAdapter.entries || []
            _historyModel.clear()
            for (const e of entries) _historyModel.append(e)
            root._nextId = historyAdapter.nextId || 1
            root.unreadCount = entries.filter(e => !e.read).length
        }
        onLoadFailed: (error) => {
            // No history file yet (first run) - start empty, it'll be
            // created on the first write.
        }
    }
}
