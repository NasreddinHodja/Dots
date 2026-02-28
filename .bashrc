#!/usr/bin/env bash

[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

bind '"\ep": history-search-backward'
bind '"\en": history-search-forward'

export EDITOR="nvim"
export VISUAL="emacsclient -nc"
export PAGER="less"
export MANPAGER="less"

export DEVKITPRO=/opt/devkitpro
export DEVKITARM=/opt/devkitpro/devkitARM
export DEVKITPPC=/opt/devkitpro/devkitPPC

source ~/.config/zsh/.node_token

export PATH="$HOME/.yarn/bin:$PATH"
export PATH="/home/nasreddin/.local/share/gem/ruby/3.0.0/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/home/nasreddin/.local/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

. "$HOME/.cargo/env"

# peon-ping quick controls
alias peon="bash /home/nasreddin/.claude/hooks/peon-ping/peon.sh"
[ -f /home/nasreddin/.claude/hooks/peon-ping/completions.bash ] && source /home/nasreddin/.claude/hooks/peon-ping/completions.bash
