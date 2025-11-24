disable r

# enable colors
autoload -U colors && colors

# history in cache dir
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.config/zsh/.zhistory

# basic auto/tab complete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)       # include hidden files

is_interactive_terminal() {
    [[ -z "$INSIDE_EMACS" ]] && \
    [[ "$TERM" != "dumb" ]] && \
    [[ -z "$VTERM" ]] && \
    [[ -t 0 ]]
}

# vi mode
# if is_interactive_terminal; then
bindkey -v
export KEYTIMEOUT=1

# keys for history substring search
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# vim keys in tab complete menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# cursor shape for different vi modes
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
    zle -K viins
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;}
# else
#     # keys for history substring search
#     bindkey '^P' history-substring-search-up
#     bindkey '^N' history-substring-search-down
#     bindkey -M vicmd '^P' history-substring-search-up
#     bindkey -M vicmd '^N' history-substring-search-down
# fi

# load aliases and shortcuts if existent
[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

# rmv automatic new line
unsetopt prompt_cr prompt_sp

# fzf completion
source /usr/share/fzf/completion.zsh

# tmux
if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
    ~/.local/bin/start_tmux
fi

# zoxide
eval "$(zoxide init zsh)"

# startship prompt
eval "$(starship init zsh)"

# tweaks for vterm
source ~/.config/zsh/zsh-vterm

# *should be last*
# load zsh-syntax-highlighting & zsh-history-substring-search
source ~/.config/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
# source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
