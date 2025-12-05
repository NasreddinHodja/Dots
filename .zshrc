source ~/.config/zsh/.zshenv
source ~/.config/zsh/.zshrc
source ~/.config/zsh/.zprofile

# pnpm
export PNPM_HOME="/home/nasreddin/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
