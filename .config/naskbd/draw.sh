#!/usr/bin/env sh

keymap draw -o diagram.svg keymap.yaml
keymap draw -s BASE -o base.svg keymap.yaml
keymap draw -s CED -o ced.svg keymap.yaml
keymap draw -s NUM -o num.svg keymap.yaml
keymap draw -s SYM -o sym.svg keymap.yaml
keymap draw -s NAV -o nav.svg keymap.yaml
keymap draw -s MOU -o mou.svg keymap.yaml
keymap draw -s FUN -o fun.svg keymap.yaml

sed -i '/class="layer-.*"/,/\/g/ {/class="label"/d}' *.svg
