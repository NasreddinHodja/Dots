// Warps the pointer to the centre of the active window after keyboard-driven focus
// changes only: directional window switches and desktop switches (including following a
// window sent to another desktop). It owns those shortcuts outright so that popups and
// focus-stealing apps, which give no way to tell they were not a keypress, can never drag
// the cursor away. A screen switch centres the pointer on the new screen instead of aiming
// at the focused window, which under focus-follows-mouse is still the one under the
// stationary pointer and so would not move it at all.

function warp(shortcut) {
    callDBus(
        "org.kde.kglobalaccel",
        "/component/kwin",
        "org.kde.kglobalaccel.Component",
        "invokeShortcut",
        shortcut
    );
}

function warpToActive() {
    warp("MoveMouseToFocus");
}

function registerSwitch(id, description, key, action) {
    registerShortcut(id, description, key, function () {
        action();
        warpToActive();
    });
}

function registerScreenSwitch(id, description, key, action) {
    registerShortcut(id, description, key, function () {
        action();
        warp("MoveMouseToCenter");
    });
}

registerSwitch("CenterCursorWindowLeft", "Switch to Window to the Left (warp cursor)",
    "Meta+H", function () { workspace.slotSwitchWindowLeft(); });
registerSwitch("CenterCursorWindowRight", "Switch to Window to the Right (warp cursor)",
    "Meta+L", function () { workspace.slotSwitchWindowRight(); });
registerSwitch("CenterCursorWindowUp", "Switch to Window Above (warp cursor)",
    "Meta+K", function () { workspace.slotSwitchWindowUp(); });
registerSwitch("CenterCursorWindowDown", "Switch to Window Below (warp cursor)",
    "Meta+J", function () { workspace.slotSwitchWindowDown(); });

registerScreenSwitch("CenterCursorScreenLeft", "Switch to Screen to the Left (warp cursor)",
    "Meta+,", function () { workspace.slotSwitchToLeftScreen(); });
registerScreenSwitch("CenterCursorScreenRight", "Switch to Screen to the Right (warp cursor)",
    "Meta+.", function () { workspace.slotSwitchToRightScreen(); });

workspace.currentDesktopChanged.connect(warpToActive);

function followWindowToDesktop(window) {
    const active = workspace.activeWindow;
    if (!active || active.internalId !== window.internalId) {
        return;
    }
    if (window.onAllDesktops) {
        return;
    }
    const desktops = window.desktops;
    if (!desktops || desktops.length !== 1) {
        return;
    }
    const target = desktops[0];
    if (!target || target.id === workspace.currentDesktop.id) {
        return;
    }
    workspace.currentDesktop = target;
}

function watchWindow(window) {
    if (!window || !window.managed) {
        return;
    }
    window.desktopsChanged.connect(function () {
        followWindowToDesktop(window);
    });
}

workspace.windowList().forEach(watchWindow);
workspace.windowAdded.connect(watchWindow);
