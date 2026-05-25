#!/usr/bin/env bash

set -e

# Get directory of this script
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

ask() {
    # ask "Message?" && do_something
    read -rp "$1 [y/N]: " ans
    [[ "$ans" =~ ^[Yy]$ ]]
}

# Hyprland
if ask "Setup hyprland?"; then
    if [ ! -d ~/.config/hypr ]; then
        echo "Setting up hyprland..."
        ln -sf "$SCRIPT_DIR/hypr" ~/.config/hypr
    else
        echo "hyprland config already exists."
    fi
fi

if ask "Setup ghostty?"; then
    if [ ! -d ~/.config/ghostty ]; then
        echo "Setting up ghostty..."
        ln -sf "$SCRIPT_DIR/ghostty" ~/.config/ghostty
    else
        echo "ghostty config already exists."
    fi
fi

# Neovim
if ask "Setup Neovim?"; then
    if [ ! -d ~/.config/nvim ]; then
        echo "Setting up Neovim..."
        ln -sf "$SCRIPT_DIR/nvim-perf" ~/.config/nvim
    else
        echo "Neovim config already exists."
    fi
fi

# Scripts
if ask "Setup Scripts folder (clone GitHub repo)?"; then
    if [ ! -d ~/Scripts ]; then
        echo "Cloning scripts..."
        git clone https://github.com/adrsha/scripts ~/Scripts
    else
        echo "\~/Scripts already exists."
    fi
fi

# fish
if ask "Setup fish shell config?"; then
    if [ ! -f ~/.config/fish/config.fish ]; then
        echo "Setting up fish..."
        mkdir -p ~/.config/fish
        ln -sf "$SCRIPT_DIR/modules/config.fish" ~/.config/fish/config.fish
    else
        echo "Fish config already exists."
    fi
fi

if ask "Setup vicinae?"; then
    if [ ! -d ~/.config/vicinae ]; then
        echo "Setting up vicinae..."
        ln -sf "$SCRIPT_DIR/vicinae" ~/.config/vicinae
    else
        echo "vicinae already exist"
    fi
fi

if ask "Setup vicinae themes?"; then
	unlink ~/.local/share/vicinae/themes || rm -rf ~/.local/share/vicinae/themes
	ln -sf "$SCRIPT_DIR/vicinae-themes" ~/.local/share/vicinae/themes
fi

# lsd
if ask "Setup lsd config?"; then
    if [ ! -d ~/.config/lsd ]; then
        echo "Setting up lsd..."
        ln -sf "$SCRIPT_DIR/lsd" ~/.config/lsd
    else
        echo "lsd config already exists."
    fi
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

