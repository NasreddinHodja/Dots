#!/usr/bin/env sh

keymap draw -o diagram.svg keymap.yaml
keymap draw -s BASE -o base.svg keymap.yaml
keymap draw -s SYM -o sym.svg keymap.yaml
keymap draw -s NAV -o nav.svg keymap.yaml
keymap draw -s MOU -o mou.svg keymap.yaml
keymap draw -s FUN -o fun.svg keymap.yaml
keymap draw -s GAM -o gam.svg keymap.yaml
keymap draw -s GNM -o gnm.svg keymap.yaml
