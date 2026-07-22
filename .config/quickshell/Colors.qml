pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    readonly property color background: adapter.background
    readonly property color surfaceContainerLowest: adapter.surface_container_lowest
    readonly property color surfaceContainerHigh: adapter.surface_container_high
    readonly property color primary: adapter.primary
    readonly property color primaryContainer: adapter.primary_container
    readonly property color secondary: adapter.secondary
    readonly property color secondaryContainer: adapter.secondary_container
    readonly property color tertiary: adapter.tertiary
    readonly property color tertiaryContainer: adapter.tertiary_container
    readonly property color onSurfaceVariant: adapter.on_surface_variant
    readonly property color outline: adapter.outline
    readonly property color error: adapter.error

    FileView {
        path: "/home/nasreddin/.config/quickshell/colors.json"
        watchChanges: true
        onFileChanged: reload()

        adapter: JsonAdapter {
            id: adapter
            property string background: "#000000"
            property string surface_container_lowest: "#000000"
            property string surface_container_high: "#000000"
            property string primary: "#000000"
            property string primary_container: "#000000"
            property string secondary: "#000000"
            property string secondary_container: "#000000"
            property string tertiary: "#000000"
            property string tertiary_container: "#000000"
            property string on_surface_variant: "#000000"
            property string outline: "#000000"
            property string error: "#000000"
        }
    }
}
