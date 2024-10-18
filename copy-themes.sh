#!/bin/bash

VIM_COLOR_DIR="$HOME/.vim/colors"
NEOVIM_COLOR_DIR="$HOME/.config/nvim/colors"

mkdir -p "$VIM_COLOR_DIR"
mkdir -p "$NEOVIM_COLOR_DIR"

cp ~/code/system-dotfiles/themes/*.vim "$VIM_COLOR_DIR"
cp ~/code/system-dotfiles/themes/*.vim "$NEOVIM_COLOR_DIR"

echo "Copied .vim files to Vim and Neovim color directories."
