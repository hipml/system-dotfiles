#!/bin/bash

DOTFILES_DIR="$HOME/code/system-dotfiles"

LINKS=(
  "$DOTFILES_DIR/emacs/init.el:$HOME/.emacs.d/init.el"
  "$DOTFILES_DIR/vim/.vimrc:$HOME/.vimrc"
  "$DOTFILES_DIR/vim/init.vim:$HOME/.config/nvim/init.vim"
  "$DOTFILES_DIR/vim/colors:$HOME/.vim/colors"
  "$DOTFILES_DIR/zsh/.zshrc:$HOME/.zshrc"
  "$DOTFILES_DIR/hooks/code-product-json.hook:/etc/pacman.d/hooks/code-product-json.hook"
  "$DOTFILES_DIR/services/my-bluetooth-fix.service:/etc/systemd/system/my-bluetooth-fix.service"
)

create_symlink() {
  local SRC="$1"
  local DEST="$2"

  if [ -e "$DEST" ] || [ -L "$DEST" ]; then
    echo "Backing up existing $DEST"
    sudo mv "$DEST" "$DEST.backup.$(date +%Y%m%d%H%M%S)"
  fi

  sudo mkdir -p "$(dirname "$DEST")"

  sudo ln -s "$SRC" "$DEST"
  echo "Linked $SRC to $DEST"
}

for link in "${LINKS[@]}"; do
  SRC="${link%%:*}"
  DEST="${link##*:}"

  if [[ "$DEST" == /etc/* ]]; then
    create_symlink "$SRC" "$DEST"
  else
    if [ -e "$DEST" ] || [ -L "$DEST" ]; then
      echo "Backing up existing $DEST"
      mv "$DEST" "$DEST.backup.$(date +%Y%m%d%H%M%S)"
    fi
    mkdir -p "$(dirname "$DEST")"
    ln -s "$SRC" "$DEST"
    echo "Linked $SRC to $DEST"
  fi
done
