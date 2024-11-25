# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

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
     ];
   };

  environment = {
    systemPackages = with pkgs; [
      neovim 
        curl
        wget
        git
        gcc      
        wl-clipboard-rs
        vulkan-tools
        vulkan-loader
        vulkan-validation-layers
        libGLU
        glxinfo
        radeontop
        supergfxctl
        file
        llvmPackages_latest.clang-tools
        llvmPackages_latest.libcxx
        lua53Packages.jsregexp
    ];
    shellAliases = {
      cp = "cp -ir";
      mv = "mv -i";
      mkdir = "mkdir -p";
      l = "lsd -L";
      ls = "lsd -L";
      la = "lsd -A";
      ll = "lsd -lA --date relative --sort git";
      lt = "lsd --tree";
      lr = "lsd -R";
      f = "cd $(fd ~ | fzf)";
      o = "~/Scripts/launch";
      n = "nvim";
      t = "nvim ~/Notes/todo.md";
      nc = "nvim ~/flakes/";
      nm = "sudo nmtui";
      ns = "nix-search -dr";
      ins = "nix-env -iA";
      uni = "nix-env --uninstall";
      rr = "sudo nixos-rebuild switch --flake $HOME/flakes/ && home-manager switch --flake ~/flakes/";
      nr = "sudo nixos-rebuild switch --flake $HOME/flakes/";
      hr = "home-manager switch --flake $HOME/flakes/";
      gpush = "cat ~/Documents/gittoken | wl-copy; git push origin $(git branch --show-current)";
    };
  };

  # PROGRAMS
  programs.hyprland.enable = true;
  programs.fish.enable = true;
  programs.npm.enable = true;
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    at-spi2-core
      boost
      cairo
      udisks
      curl
      dbus
      fmt
      fuse3
      glib
      glibc
      graphite2
      gtk3
      gtk3-x11
      harfbuzz
      libdbusmenu-gtk3
      libepoxy
      libselinux
      libsepol
      libstdcxx5
      libxkbcommon
      openssl                                                                                                                                                                                                                           
      pango
      pcre
      qt5.qtbase
      qt5.qtwayland
      stdenv.cc.cc
      stdenv.cc.cc.lib
      util-linux
      xorg.libX11
      xorg.libXcursor                                                                                                                                                                                                                   
      xorg.libXdmcp
      xorg.libXi                                                                                                                                                                                                                        
      xorg.libXtst
      xorg.libxcb                                                                                                                                                                                                                       
      zlib
      ];

  nix.settings = {
     experimental-features = ["nix-command" "flakes"];
  };

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

