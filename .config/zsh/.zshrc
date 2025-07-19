# set +x

disable r

# Enable colors and change prompt:
autoload -U colors && colors
# PS1="%B%{$fg[red]%}[%{$fg[magenta]%}%~%{$fg[red]%}]
# %{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M%{$reset_color%}
#  -> $%b "
# PS1="%B%{$fg[red]%}  : %{$fg[magenta]%}%~%{$fg[red]%} : %{$reset_color%}
# ┏ %B%{$fg[red]%}[%{$fg[blue]%}%n%{$fg[green]%}@%{$fg[yellow]%}%M%{$fg[red]%}]%{$reset_color%}
# ┗━%B $%b "

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.config/zsh/.zhistory

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)       # Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys for history substring search
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# vi-append-x-selection () { RBUFFER=$(xsel -o -p </dev/null)$RBUFFER; }
# zle -N vi-append-x-selection
# bindkey -a '^X' vi-append-x-selection
# vi-yank-x-selection () { print -rn -- $CUTBUFFER | xsel -i -p; }
# zle -N vi-yank-x-selection
# bindkey -a '^Y' vi-yank-x-selection
# # Edit line in vim with ctrl-e:
# autoload edit-command-line; zle -N edit-command-line
# bindkey '^e' edit-command-line

# Load aliases and shortcuts if existent.
[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

# Load zsh-syntax-highlighting; should be last.
source ~/.config/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
#source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# remove automatic new line
unsetopt prompt_cr prompt_sp

# fzf completion
source /usr/share/fzf/completion.zsh

# NODE_AUTH
source ~/.config/zsh/.node_token

# MAN PAGER
# export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANPAGER=less

# ruby gems
export PATH=$PATH:/home/nasreddin/.local/share/gem/ruby/3.0.0/bin

# bin
export PATH=$PATH:/home/nasreddin/.local/bin

# yarn bins
export PATH="$HOME/.yarn/bin:$PATH"

# tmux
if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
  ~/.local/bin/start_tmux
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# zoxide
eval "$(zoxide init zsh)"

# startship prompt
eval "$(starship init zsh)"

# set +x
