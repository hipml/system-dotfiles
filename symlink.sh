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
  "$DOTFILES_DIR/kernel/psmouse.conf:/etc/modprobe.d/psmouse.conf"
  "$DOTFILES_DIR/udev/90-backlight.rules:/etc/udev/rules.d/90-backlight.rules"
)

for theme_file in "$DOTFILES_DIR/emacs/themes/"*.el; do
  theme_name=$(basename "$theme_file")
  LINKS+=("$theme_file:$HOME/.emacs.d/themes/$theme_name")
done

create_symlink() {
  local SRC="$1"
  local DEST="$2"

  if [ -e "$DEST" ] || [ -L "$DEST" ]; then
    echo "Backing up existing $DEST"
    if [[ "$DEST" == /etc/* ]]; then
      sudo mv "$DEST" "$DEST.backup.$(date +%Y%m%d%H%M%S)"
    else
      mv "$DEST" "$DEST.backup.$(date +%Y%m%d%H%M%S)"
    fi
  fi

  if [[ "$DEST" == /etc/* ]]; then
    sudo mkdir -p "$(dirname "$DEST")"
    sudo ln -s "$SRC" "$DEST"
  else
    mkdir -p "$(dirname "$DEST")"
    ln -s "$SRC" "$DEST"
  fi
  echo "Linked $SRC to $DEST"
}

for link in "${LINKS[@]}"; do
  SRC="${link%%:*}"
  DEST="${link##*:}"

  create_symlink "$SRC" "$DEST"
done
