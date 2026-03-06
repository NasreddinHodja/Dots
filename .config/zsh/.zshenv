export EDITOR="nvim"
export VISUAL="emacsclient -nc"
export PAGER="less"
export MANPAGER="less"

export DEVKITPRO=/opt/devkitpro
export DEVKITARM=/opt/devkitpro/devkitARM
export DEVKITPPC=/opt/devkitpro/devkitPPC

export CHROME_EXECUTABLE=/usr/sbin/chromium

export ANDROID_SDK_ROOT="$HOME/.local/src/Android"
export ANDROID_HOME="$ANDROID_SDK_ROOT"

source ~/.config/zsh/.node_token

# XDG tool homes
export CARGO_HOME="$HOME/.local/share/cargo"
export RUSTUP_HOME="$HOME/.local/share/rustup"
export BUN_INSTALL="$HOME/.local/share/bun"
export NVM_DIR="$HOME/.local/share/nvm"
export DOCKER_CONFIG="$HOME/.config/docker"
export GNUPGHOME="$HOME/.local/share/gnupg"
export IPYTHONDIR="$HOME/.config/ipython"
export JUPYTER_CONFIG_DIR="$HOME/.config/jupyter"
export NPM_CONFIG_USERCONFIG="$HOME/.config/npm/npmrc"
export WGETRC="$HOME/.config/wget/wgetrc"

export PATH="$HOME/.local/share/yarn/bin:$PATH"
export PATH="/home/nasreddin/.local/share/gem/ruby/3.0.0/bin:$PATH"
export PATH="$CARGO_HOME/bin:$PATH"
export PATH="/home/nasreddin/.local/bin:$PATH"
export PATH="$HOME/.local/src/flutter/bin:$PATH"
export PATH="$BUN_INSTALL/bin:$PATH"

[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

. "$CARGO_HOME/env"

# pnpm
export PNPM_HOME="/home/nasreddin/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# bun completions
[ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"
