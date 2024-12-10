# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, inputs, ... }:

{

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; 
  networking.networkmanager.enable = true;  

  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };
  
  users.users.chilly = {
    isNormalUser = true;
    initialPassword = "123";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
      shell = pkgs.fish;
    packages = with pkgs; [
    tldr

# System Utilities
    blueberry
      brightnessctl
      gnumake
      nix-search-cli
      xorg.xset
      udiskie
      libnotify
      killall
      imagemagick

# System Monitoring
      btop
      neofetch

# Terminal Utilities
      alacritty
      fzf
      fd
      bat
      ripgrep
      zoxide
      lsd
      clipse
      ueberzugpp

# Development Tools
      cargo
      cmake
      ninja
      python3

# Programming Languages and Toolchains
      flutter

# Language Servers
      vscode-langservers-extracted
      cpplint
      lua-language-server
      pyright
      typescript-language-server
      nil
      jq-lsp
      bash-language-server
      rust-analyzer

# Plugins
      fishPlugins.tide

# Qt and UI Libraries
      qt5.full

# Desktop Environment Tools
      inputs.ags.packages."${system}".default # for hyprpanel
      hyprpanel
      swww
      hyprpicker
      grim
      slurp
      playerctl

# gtk
      gtk3
      gtk4
      nwg-look

# Multimedia
      mpv
      imv

# Compression and Archiving
      unzip

# Browsers and Internet
      inputs.zen-browser.packages."${system}".default
      qbittorrent

# Game Engines
      godot_4
    ];
  };
  
  security.sudo.extraRules= [
  {  users = [ "chilly" ];
    commands = [
    { command = "ALL" ;
      options= [ "NOPASSWD" ]; 
    }
    ];
  }
  ];
  environment = {
    systemPackages = with pkgs; [
      neovim 
        curl
        wget
        git
        gcc      
        lsd
        wl-clipboard-rs
        file
        llvmPackages_latest.libcxx
        lua53Packages.jsregexp
    ];
  };

# PROGRAMS
  programs.hyprland.enable = true;
  programs.fish.enable = true;
  programs.npm.enable = true;
  programs.adb.enable = true;
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    # at-spi2-core
      # boost
      # cairo
      # udisks
      # curl
    #   dbus
    #   fmt
      # fuse3
    #   glib
    #   glibc
    #   graphite2
    #   gtk3
    #   gtk3-x11
    #   harfbuzz
    #   libepoxy
    #   libselinux
    #   libsepol
    #   libxkbcommon
    #   openssl                                                                                                                                                                                                                           
    #   pango
    #   pcre
    #   qt5.qtbase
    #   qt5.qtwayland
    #   stdenv.cc.cc
    #   stdenv.cc.cc.lib
    #   util-linux
    #   xorg.libX11
    #   xorg.libXcursor                                                                                                                                                                                                                   
    #   xorg.libXdmcp
    #   xorg.libXi                                                                                                                                                                                                                        
    #   xorg.libXtst
    #   xorg.libxcb                                                                                                                                                                                                                       
    #   zlib
      ];

  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
  ];

  # Optional: System-wide GTK theme configuration
  environment.etc."gtk-3.0/settings.ini" = {
    text = ''
      [Settings]
      gtk-theme-name=Graphite-Dark
    '';
  };

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
  };
  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot
  nixpkgs.overlays = [ inputs.hyprpanel.overlay ];
# This option defines the first version of NixOS you have installed on this particular machine,
# and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
#
# Most users should NEVER change this value after the initial install, for any reason,
# even if you've upgraded your system to a new NixOS release.
#
# This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
# so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
# to actually do that.
#
# This value being lower than the current NixOS release does NOT mean your system is
# out of date, out of support, or vulnerable.
#
# Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
# and migrated your data accordingly.
#
# For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.05"; # Did you read the comment?

}

