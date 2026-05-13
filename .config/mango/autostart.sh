#!/usr/bin/env sh

set +e

dbus-update-activation-environment --systemd --all &

dunst >/dev/null 2>&1 &
awww-daemon >/dev/null 2>&1 &
/home/nasreddin/Prog/Waybar/build/waybar >/dev/null 2>&1 &
blueman-applet >/dev/null 2>&1 &
nm-applet >/dev/null 2>&1 &
flameshot >/dev/null 2>&1 &
/home/nasreddin/.local/bin/main-emacsclient >/dev/null 2>&1 &
