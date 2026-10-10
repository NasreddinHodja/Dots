// kwin half of dev-layout. dev-layout prepends the consts it needs:
// MAIN_OUTPUT, DESKTOP_INDEX, FOCUS_SIDE, LEFT_CLASS, RIGHT_TITLE.

function mainOutput() {
    var found = null;
    workspace.screens.forEach(function (s) {
        if (s.name === MAIN_OUTPUT) found = s;
    });
    return found || workspace.activeScreen;
}

function targetDesktop() {
    var i = Math.max(0, Math.min(DESKTOP_INDEX, workspace.desktops.length - 1));
    return workspace.desktops[i];
}

// with kwin's PerOutputVirtualDesktops each output has its own current desktop,
// and `workspace.currentDesktop = d` only moves the *active* one. so name the
// output explicitly. (note the arg order: desktop first, output second.)
function showDesktopOn(out, desk) {
    if (typeof workspace.setCurrentDesktopForScreen === "function")
        workspace.setCurrentDesktopForScreen(desk, out);
    else
        workspace.currentDesktop = desk;
}

function sideOf(w) {
    if (!w || !w.normalWindow) return null;
    var cls = String(w.resourceClass).toLowerCase();
    if (cls === LEFT_CLASS) return "left";
    if (cls.indexOf("emacs") !== -1 &&
        String(w.caption).indexOf(RIGHT_TITLE) !== -1) return "right";
    return null;
}

var pane = { left: null, right: null };

function focusPreferred() {
    var w = pane[FOCUS_SIDE];
    if (w !== null) workspace.activeWindow = w;
}

// the tile slots act on the active window, on whatever output it's already on.
// so desktop, output and focus have to be set first, in that order.
function place(w) {
    var side = sideOf(w);
    if (side === null) return;

    w.onAllDesktops = false;
    w.desktops = [targetDesktop()];
    if (w.fullScreen) w.fullScreen = false;

    var out = mainOutput();
    if (w.output !== out) workspace.sendClientToScreen(w, out);

    workspace.activeWindow = w;
    if (side === "left") workspace.slotWindowQuickTileLeft();
    else                 workspace.slotWindowQuickTileRight();

    pane[side] = w;
    focusPreferred();   // the tile call stole focus
}

showDesktopOn(mainOutput(), targetDesktop());
workspace.windowList().forEach(place);   // whatever's already open
workspace.windowAdded.connect(place);    // windows map after the spawns return
