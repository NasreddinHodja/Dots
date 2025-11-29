#!/usr/bin/env sh

notify() {
    if "$@"; then
        dunstify "✔️ Success" "$*"
    else
        dunstify "❌ Failed" "$*"
    fi
}

"$HOME/.config/screenlayout.sh"
feh --bg-fill "$(printf '%s\n' ~/.config/wp.* | head -n 1)" &
emacs --bg-daemon &
setxkbmap -layout us -variant intl
picom &

$HOME/.local/bin/main-emacsclient

xset s off
xset s noblank
xset -dpms
xset dpms 0 0 0
xscreensaver -no-splash &
nm-applet &
