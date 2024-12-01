#!/usr/bin/env bash

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

# Hyprland
if [ ! -f ~/.config/hypr/hyprland.conf ]; then
  echo "Setting up hyprland"
  if [ ! -d ~/.config/hypr ]; then
    mkdir -p ~/.config/hypr
  fi
  ln -sf ~/flakes/modules/hyprland.conf ~/.config/hypr/hyprland.conf
fi

# Alacritty
if [ ! -f ~/.config/alacritty/alacritty.toml ]; then
  echo "Setting up alacritty"
  if [ ! -d ~/.config/alacritty ]; then
    mkdir -p ~/.config/alacritty
  fi
  ln -sf ~/flakes/modules/alacritty.toml ~/.config/alacritty/alacritty.toml
fi

