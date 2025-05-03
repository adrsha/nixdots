#!/usr/bin/env bash

# Hyprland
if [ ! -f ~/.config/hypr/hyprland.conf ]; then
  echo "Setting up hyprland"
  if [ ! -d ~/.config/hypr ]; then
    mkdir -p ~/.config/hypr
  fi
  ln -sf ~/flakes/modules/hyprland.conf ~/.config/hypr/hyprland.conf
fi

# Neovim
if [ ! -d ~/.config/nvim ]; then
  echo "Setting up neovim"
  ln -sf ~/flakes/nvim-perf ~/.config/nvim
fi

# Scripts
if [ ! -d ~/Scripts ]; then
  echo "Setting up scripts"
  git clone https://github.com/adrsha/scripts ~/Scripts
fi

# Alacritty
if [ ! -f ~/.config/alacritty/alacritty.toml ]; then
  echo "Setting up alacritty"
  if [ ! -d ~/.config/alacritty ]; then
    mkdir -p ~/.config/alacritty
  fi
  ln -sf ~/flakes/modules/alacritty.toml ~/.config/alacritty/alacritty.toml
fi

# fish
if [ ! -f ~/.config/fish/config.fish ]; then
  echo "Setting up fish"
  if [ ! -d ~/.config/fish ]; then
    mkdir -p ~/.config/fish
  fi
  ln -sf ~/flakes/modules/config.fish ~/.config/fish/config.fish
  # if tide is-installed; then
    # tide configure
  # fi
fi

# lsd
if [ ! -d ~/.config/lsd ]; then
  echo "Setting up lsd"
  ln -sf ~/flakes/lsd ~/.config/lsd
fi

# waybar
if [ ! -d ~/.config/waybar ]; then
  echo "Setting up waybar"
  ln -sf ~/flakes/waybar ~/.config/waybar
fi

# # ags
# if [ ! -d ~/.config/ags ]; then
#   echo "Setting up ags"
#   ln -sf ~/flakes/ags ~/.config/ags
# fi
#
# Hardware Configuration
cp /etc/nixos/hardware-configuration.nix ~/flakes/hardware-configuration.nix
