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
setxkbmap -layout us -variant intl &
picom &

emacsclient --alternate-editor= --create-frame -n

xscreensaver -no-splash &
nm-applet &
