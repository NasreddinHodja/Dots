#!/usr/bin/env sh

set +e

dbus-update-activation-environment --systemd --all
# some env can't auto run the portal, so need this
/usr/lib/xdg-desktop-portal-wlr  >/dev/null 2>&1 &

# notify
# swaync -c ~/.config/mango/swaync/config.jsonc -s ~/.config/mango/swaync/style.css >/dev/null 2>&1 &
dunst >/dev/null 2>&1 &

# wallpaper
awww-daemon >/dev/null 2>&1 &

# bar
/home/nasreddin/Prog/Waybar/build/waybar >/dev/null 2>&1 &

# bluetooth
blueman-applet >/dev/null 2>&1 &

# network
nm-applet >/dev/null 2>&1 &

# screeshot
flameshot >/dev/null 2>&1 &
# Permission authentication
# /usr/lib/xfce-polkit/xfce-polkit >/dev/null 2>&1 &

# main emacs
/home/nasreddin/.local/bin/main-emacsclient >/dev/null 2>&1 &
