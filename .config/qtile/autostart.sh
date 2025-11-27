#!/usr/bin/env sh

$HOME/.config/screenlayout.sh
emacs --fg-daemon &
setxkbmap -layout us -variant intl
picom &

main-emacsclient

xscreensaver -no-splash &
dunst &
