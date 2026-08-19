source ~/.config/zsh/.zshenv
source ~/.config/zsh/.zshrc
source ~/.config/zsh/.zprofile

export NVM_DIR="$HOME/.local/share/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# bun completions
[ -s "/home/nasreddin/.local/share/bun/_bun" ] && source "/home/nasreddin/.local/share/bun/_bun"
