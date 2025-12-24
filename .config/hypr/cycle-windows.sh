#!/usr/bin/env sh

ACTIVE="$(hyprctl activewindow -j)"

if echo "$ACTIVE" | jq -e '.grouped | length > 1' >/dev/null; then
    hyprctl dispatch changegroupactive f
    # notify-send "Hyprland cycle" "Group → next window"
elif echo "$ACTIVE" | jq -e '.floating == true' >/dev/null; then
    hyprctl dispatch cyclenext floating
    # notify-send "Hyprland cycle" "Floating → next window"
else
    hyprctl dispatch cyclenext tiled
    # notify-send "Hyprland cycle" "Tiled → next window"
fi
