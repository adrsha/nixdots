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
if ask "Setup Hyprland?"; then
    if [ ! -f ~/.config/hypr/hyprland.conf ]; then
        echo "Setting up Hyprland..."
        mkdir -p ~/.config/hypr
        ln -sf "$SCRIPT_DIR/modules/hyprland.conf" ~/.config/hypr/hyprland.conf
    else
        echo "Hyprland is already configured."
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

# lsd
if ask "Setup lsd config?"; then
    if [ ! -d ~/.config/lsd ]; then
        echo "Setting up lsd..."
        ln -sf "$SCRIPT_DIR/lsd" ~/.config/lsd
    else
        echo "lsd config already exists."
    fi
fi

# waybar
if ask "Setup waybar config?"; then
    if [ ! -d ~/.config/waybar ]; then
        echo "Setting up waybar..."
        ln -sf "$SCRIPT_DIR/waybar" ~/.config/waybar
    else
        echo "Waybar config already exists."
    fi
fi

# mako
if ask "Setup mako config?"; then
    if [ ! -d ~/.config/mako ]; then
        echo "Setting up mako..."
        ln -sf "$SCRIPT_DIR/mako" ~/.config/mako
    else
        echo "Mako config already exists."
    fi
fi


# Wezterm
if ask "Setup wezterm config?"; then
    if [ ! -d ~/.config/wezterm ]; then
        echo "Setting up wezterm..."
        ln -sf "$SCRIPT_DIR/wezterm" ~/.config/wezterm
    else
        echo "wezterm config already exists."
    fi
fi

# Kitty
if ask "Setup Kitty?"; then
    if [ ! -f ~/.config/kitty/kitty.conf ]; then
        echo "Setting up Kitty..."
        mkdir -p ~/.config/kitty
        ln -sf "$SCRIPT_DIR/modules/kitty.conf" ~/.config/kitty/kitty.conf
    else
        echo "Kitty is already configured."
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
