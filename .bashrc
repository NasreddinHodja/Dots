#!/usr/bin/env bash

[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

bind '"\ep": history-search-backward'
bind '"\en": history-search-forward'
