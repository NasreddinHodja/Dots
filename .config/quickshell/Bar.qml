import QtQuick
import QtQuick.Layouts
import Quickshell

PanelWindow {
    id: bar

    screen: {
        for (let i = 0; i < Quickshell.screens.length; i++) {
            if (Quickshell.screens[i].name === "DP-3") return Quickshell.screens[i]
        }
        return Quickshell.screens[0]
    }

    anchors {
        bottom: true
        left: true
        right: true
    }

    margins {
        bottom: 6
        left: 200
        right: 200
    }

    implicitHeight: 24
    color: Colors.surfaceContainerLowest

    RowLayout {
        anchors.left: parent.left
        anchors.leftMargin: 8
        anchors.verticalCenter: parent.verticalCenter
        spacing: 14

        Workspaces {}
        LayoutIndicator {}
        ActiveWindow {}
        MinimizedWindows {}
    }

    RowLayout {
        anchors.right: parent.right
        anchors.rightMargin: 8
        anchors.verticalCenter: parent.verticalCenter
        spacing: 14

        Tray { barWindow: bar }
        MprisWidget {}
        KeyboardState {}
        SysMon {}
        NotificationCenter { barWindow: bar }
        Pulseaudio {}
        Clock { barWindow: bar }
    }
}
