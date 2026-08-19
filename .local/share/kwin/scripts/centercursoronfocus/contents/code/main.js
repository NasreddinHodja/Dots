function centerCursorOnActiveWindow() {
    callDBus(
        "org.kde.kglobalaccel",
        "/component/kwin",
        "org.kde.kglobalaccel.Component",
        "invokeShortcut",
        "MoveMouseToFocus"
    );
}

workspace.windowActivated.connect(function (window) {
    if (window) {
        centerCursorOnActiveWindow();
    }
});
