#!/usr/bin/env bash

set -e

# Get directory of this script
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

# Mode: pass "init" as the first argument to force-replace existing
# configs/links without prompting for confirmation on the overwrite.
#   ./setup.sh init
MODE="${1:-}"

ask() {
    # ask "Message?" && do_something
    read -rp "$1 [y/N]: " ans
    [[ "$ans" =~ ^[Yy]$ ]]
}

# link_target <target_path> <source_path> <name>
# Handles the "does it already exist" check, with init-mode override.
# Removes any existing file/dir/symlink at target when replacing.
link_target() {
    local target="$1"
    local source="$2"
    local name="$3"

    if [ -e "$target" ] || [ -L "$target" ]; then
        if [ "$MODE" == "init" ]; then
            echo "Replacing existing $name config..."
            rm -rf "$target"
            ln -sf "$source" "$target"
        else
            echo "$name config already exists."
        fi
    else
        echo "Setting up $name..."
        ln -sf "$source" "$target"
    fi
}

# Hyprland
if ask "Setup hyprland?"; then
    link_target ~/.config/hypr "$SCRIPT_DIR/hypr" "hyprland"
fi

if ask "Setup ghostty?"; then
    link_target ~/.config/ghostty "$SCRIPT_DIR/ghostty" "ghostty"
fi

# Neovim
if ask "Setup Neovim?"; then
    link_target ~/.config/nvim "$SCRIPT_DIR/nvim-perf" "Neovim"
fi

# Scripts
if ask "Setup Scripts folder (clone GitHub repo)?"; then
    if [ -e ~/Scripts ] || [ -L ~/Scripts ]; then
        if [ "$MODE" == "init" ]; then
            echo "Replacing existing ~/Scripts..."
            rm -rf ~/Scripts
            git clone https://github.com/adrsha/scripts ~/Scripts
        else
            echo "~/Scripts already exists."
        fi
    else
        echo "Cloning scripts..."
        git clone https://github.com/adrsha/scripts ~/Scripts
    fi
fi

# fish
if ask "Setup fish shell config?"; then
    mkdir -p ~/.config/fish
    link_target ~/.config/fish/config.fish "$SCRIPT_DIR/modules/config.fish" "fish"
fi

if ask "Setup vicinae?"; then
    link_target ~/.config/vicinae "$SCRIPT_DIR/vicinae" "vicinae"
fi

if ask "Setup vicinae themes?"; then
    unlink ~/.local/share/vicinae/themes 2>/dev/null || rm -rf ~/.local/share/vicinae/themes
    ln -sf "$SCRIPT_DIR/vicinae-themes" ~/.local/share/vicinae/themes
fi

# lsd
if ask "Setup lsd config?"; then
    link_target ~/.config/lsd "$SCRIPT_DIR/lsd" "lsd"
fi

# Hardware configuration (only for NixOS)
if grep -qi "nixos" /etc/os-release 2>/dev/null; then
    if ask "Copy hardware-configuration.nix to flakes dir?"; then
        echo "Copying hardware configuration..."
        cp /etc/nixos/hardware-configuration.nix "$SCRIPT_DIR/hardware-configuration.nix"
    fi
else
    echo "Not a NixOS system — skipping hardware configuration."
fi
