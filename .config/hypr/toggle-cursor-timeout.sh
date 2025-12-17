#!/usr/bin/env sh

STATE="$HOME/.cache/hypr-cursor-timeout"

if [ -f "$STATE" ]; then
    hyprctl keyword cursor:inactive_timeout 0
    rm "$STATE"
else
    hyprctl keyword cursor:inactive_timeout 1
    touch "$STATE"
fi
