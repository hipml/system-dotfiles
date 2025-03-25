#!/bin/bash

DOTFILES_DIR="$HOME/code/system-dotfiles"

LINKS_COMMON=(
    "$DOTFILES_DIR/emacs/init.el:$HOME/.emacs.d/init.el"
    "$DOTFILES_DIR/vim/.vimrc:$HOME/.vimrc"
    "$DOTFILES_DIR/vim/init.lua:$HOME/.config/nvim/init.lua"
    "$DOTFILES_DIR/vim/colors:$HOME/.vim/colors"
    "$DOTFILES_DIR/zsh/.zshrc:$HOME/.zshrc"
    "$DOTFILES_DIR/services/my-bluetooth-fix.service:/etc/systemd/system/my-bluetooth-fix.service"
    "$DOTFILES_DIR/sddm/sddm.conf:/etc/sddm.conf"
    "$DOTFILES_DIR/sddm/10-wayland.conf:/etc/sddm.conf.d/10-wayland.conf"
    "$DOTFILES_DIR/kernel/disable_watchdog.conf:/etc/sysctl.d/disable_watchdog.conf"
    "$DOTFILES_DIR/services/reflector.service:/etc/systemd/system/reflector.service"
    "$DOTFILES_DIR/services/reflector.timer:/etc/systemd/system/reflector.timer"
    "$DOTFILES_DIR/kitty/kitty.conf:$HOME/.config/kitty/kitty.conf"
)

# specific files for Thinkpad T480
# mostly things like trackpad, fingerprint reader on login screen, backlight....
if [ "$(uname -n)" = "archpad" ]; then
    LINKS+=(
        "$DOTFILES_DIR/kernel/psmouse.conf:/etc/modprobe.d/psmouse.conf"
        "$DOTFILES_DIR/udev/90-backlight.rules:/etc/udev/rules.d/90-backlight.rules"
        "$DOTFILES_DIR/pam/ssdm:/etc/pam.d/sddm"
        "$DOTFILES_DIR/kernel/throttlestop:/usr/local/bin/throttlestop"
    )
    # desktop specific
elif [ "$(uname -n)" = "Archon" ]; then
    LINKS+=(
        "$DOTFILES_DIR/kernel/disable-sp5100-watchdog.conf:/etc/modprobe.d/disable-sp5100-watchdog.conf"
        "$DOTFILES_DIR/services/s2idle-sleep.service:/etc/systemd/system/s2idle-sleep.service"
    )
fi

LINKS=("${LINKS_COMMON[@]}" "${LINKS[@]}")

for theme_file in "$DOTFILES_DIR/emacs/themes/"*.el; do
    theme_name=$(basename "$theme_file")
    LINKS+=("$theme_file:$HOME/.emacs.d/themes/$theme_name")
done

create_symlink() {
    local SRC="$1"
    local DEST="$2"
    
    # check if DEST is already a symlink pointing to the correct SRC
    if [ -L "$DEST" ]; then
        local current_target
        if [[ "$DEST" == /etc/* ]]; then
            current_target=$(sudo readlink "$DEST")
        else
            current_target=$(readlink "$DEST")
        fi
        
        if [ "$current_target" = "$SRC" ]; then
            echo "✓ Link already exists and is correct: $DEST -> $SRC"
            return 0
        fi
    fi

    # if we get here, either the symlink doesn't exist or points somewhere else
    if [ -e "$DEST" ] || [ -L "$DEST" ]; then
        echo "⚠ Backing up existing $DEST"
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
    echo "✓ Created link: $DEST -> $SRC"
}

for link in "${LINKS[@]}"; do
    SRC="${link%%:*}"
    DEST="${link##*:}"

    create_symlink "$SRC" "$DEST"
done

echo "✓ Dotfiles setup complete!"
