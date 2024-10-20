#!/bin/bash

DOTFILES_DIR="$HOME/code/system-dotfiles"
LINKS=(
  "$DOTFILES_DIR/emacs/init.el:$HOME/.emacs.d/init.el"
)

for link in "${LINKS[@]}"; do
  SRC="${link%%:*}"
  DEST="${link##*:}"

  if [ -f "$DEST" ] || [ -d "$DEST" ]; then
    mv "$DEST" "$DEST.backup.$(date +%Y%m%d%H%M%S)"
  fi

  mkdir -p "$(dirname "$DEST")"

  ln -s "$SRC" "$DEST"
  echo "Linked $SRC to $DEST"
done
