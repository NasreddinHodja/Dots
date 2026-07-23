import QtQuick
import Quickshell
import Quickshell.Wayland

// Instantiate via a Loader keyed off the caller's open/closed state, not by
// toggling `visible` - a persistent hidden overlay window leaves a dead
// click-eating strip over the anchor widget even while closed.
PanelWindow {
    id: root

    property var anchorItem: null
    property var barWindow: null
    property string namespace: ""
    readonly property int padding: 12

    default property alias content: contentHost.data
    property alias cardWidth: card.width
    property alias cardHeight: card.height

    signal dismissed()

    WlrLayershell.namespace: root.namespace
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    screen: {
        for (let i = 0; i < Quickshell.screens.length; i++) {
            if (Quickshell.screens[i].name === "DP-3") return Quickshell.screens[i]
        }
        return Quickshell.screens[0]
    }

    anchors { top: true; bottom: true; left: true; right: true }
    exclusionMode: ExclusionMode.Ignore
    color: "transparent"
    visible: true

    MouseArea {
        anchors.fill: parent
        focus: true
        Keys.onEscapePressed: root.dismissed()
        Component.onCompleted: forceActiveFocus()
        onClicked: root.dismissed()
    }

    Rectangle {
        id: card
        // Live bindings: root.width/height aren't negotiated by the
        // compositor until this surface is shown, so a one-shot snapshot
        // would freeze at a stale placeholder value.
        x: {
            if (!root.anchorItem || !root.barWindow) return 0
            const widgetCenterX = root.barWindow.margins.left + root.anchorItem.mapToItem(null, 0, 0).x + root.anchorItem.width / 2
            return Math.max(0, Math.min(widgetCenterX - width / 2, root.width - width))
        }
        y: root.barWindow ? (root.height - (root.barWindow.margins.bottom + root.barWindow.height) - height - 6) : 0
        color: Colors.surfaceContainerLowest
        border.width: 0
        border.color: Colors.outline

        // Swallows clicks so they don't fall through to the dismiss
        // MouseArea behind the card.
        MouseArea {
            anchors.fill: parent
        }

        Item {
            id: contentHost
            anchors.fill: parent
            anchors.margins: root.padding
        }
    }
}
