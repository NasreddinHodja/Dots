function centerCursorOnActiveWindow() {
    callDBus(
        "org.kde.kglobalaccel",
        "/component/kwin",
        "org.kde.kglobalaccel.Component",
        "invokeShortcut",
        "MoveMouseToFocus"
    );
}

function cursorInsideWindow(window) {
    const cursor = workspace.cursorPos;
    const geo = window.frameGeometry;
    return cursor.x >= geo.x && cursor.x < geo.x + geo.width &&
        cursor.y >= geo.y && cursor.y < geo.y + geo.height;
}

workspace.windowActivated.connect(function (window) {
    // Skip when the cursor is already inside the newly focused window: that
    // means the mouse itself caused the focus change (click / focus-follows-
    // mouse), and jumping the cursor to center would yank it away from where
    // the user just clicked.
    if (window && !cursorInsideWindow(window)) {
        centerCursorOnActiveWindow();
    }
});
