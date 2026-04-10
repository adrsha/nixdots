# Dotfiles

Personal dotfiles managed with symlinks and a simple setup script.

## Setup

Clone the repository:

```bash
git clone https://github.com/adrsha/dots ~/.dots
cd ~/.dots
```

Run the setup script:

```bash
chmod +x setup.sh
./setup.sh
```

The script will prompt you for each component before creating symlinks or cloning resources.

## What it sets up

* Hyprland config → `~/.config/hypr/hyprland.conf`
* Ghostty config → `~/.config/ghostty`
* Neovim config → `~/.config/nvim`
* Fish config → `~/.config/fish/config.fish`
* lsd config → `~/.config/lsd`
* Scripts repo → `~/Scripts` (cloned from GitHub)
* NixOS hardware config (optional, only on NixOS)

## Notes

* Existing configs are not overwritten. If a target already exists, the script skips it.
* Symlinks are created using absolute paths based on the repository location.
* Designed primarily for NixOS, but most components work on any Linux system.

## NixOS

If running on NixOS:

* You can copy your current hardware configuration into this repo via the setup script.
* `flake.nix` and `configuration.nix` are included for system configuration.

## Customization

Edit files inside this repository and rerun the script or re-link manually:

```bash
ln -sf <source> <target>
```
