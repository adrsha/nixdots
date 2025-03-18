# ╭───────────────────────────────────────────────────────────────────────────────╮
# │                       Main NixOS Configuration File                           │
# │   Defines system configuration, specifying what to install and configure.     │
# ╰───────────────────────────────────────────────────────────────────────────────╯

{ config, lib, pkgs, inputs, ... }:

{
  # ╭─────────────────────────────╮
  # │ Boot Configuration          │
  # ╰─────────────────────────────╯
  boot.loader.systemd-boot.enable = true;  # Uses systemd-boot as the bootloader
  boot.loader.efi.canTouchEfiVariables = true;  # Allow modifying EFI variables

  # ╭────────────────────────────────────────────────────────╮
  # │ Kernel Parameters                                      │
  # ╰────────────────────────────────────────────────────────╯
  boot.kernelParams = [
    "nvidia_drm.modeset=1" 
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"  # Better system stability with NVIDIA
  ];

  # ╭──────────────────────────────╮
  # │ Network Configuration        │
  # ╰──────────────────────────────╯
  networking.hostName = "nixos";  # Sets the hostname of the system
  networking.networkmanager.enable = true;  # Enables NetworkManager for network management
  time.timeZone = "Asia/Kathmandu";
  
  # ╭────────────────────────────────────╮
  # │ Firewall Configuration             │
  # ╰────────────────────────────────────╯
  networking.firewall = {
    enable = false;
    allowedTCPPorts = [ 80 443 ];  # Common web ports
    allowedUDPPorts = [ 53 ];  # DNS
  };

  # ╭──────────────────────────────────────────────────────────╮
  # │ Audio Configuration                                      │
  # ╰──────────────────────────────────────────────────────────╯
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;  # For 32-bit application support
    pulse.enable = true;
  };

  # ╭──────────────────────────────────────────────────────────────────╮
  # │ User Configuration                                               │
  # ╰──────────────────────────────────────────────────────────────────╯
  users.users.chilly = {
    isNormalUser = true;
    initialPassword = "changeme"; 
    extraGroups = [ "wheel" ];  # Enable 'sudo' for the user
    shell = pkgs.fish;  # Set the default shell to fish
    packages = with pkgs; [
      tldr  # Command documentation tool
      blueberry  # Bluetooth manager
      brightnessctl  # Brightness control utility
      gnumake  # GNU Make
      nix-search-cli  # CLI tool for searching Nix packages
      xorg.xset  # X server settings utility
      udiskie  # Automount removable media
      libnotify  # Desktop notifications library
      killall  # Process termination utility
      imagemagick  # Image manipulation

      # System Monitoring
      btop  # System monitor
      neofetch  # System info display

      # Terminal Utilities
      alacritty  # GPU-accelerated terminal emulator
      fzf  # Fuzzy finder
      fd  # Fast alternative to find
      bat  # Cat clone with syntax highlighting
      ripgrep  # Fast grep alternative
      zoxide  # Smarter cd command
      lsd  # Modern ls alternative
      clipse  # Clipboard manager
      ueberzugpp  # Image viewer for terminal

      # Development Tools
      cargo  # Rust package manager
      cmake  # Cross-platform build system
      ninja  # Build system focused on speed
      python3  # Python programming language

      # Programming Languages and Toolchains
      flutter  # UI toolkit for mobile, web, and desktop

      # Language Servers for code completion and analysis
      vscode-langservers-extracted
      cpplint
      lua-language-server
      pyright
      typescript-language-server
      nil  # Nix language server
      jq-lsp
      bash-language-server
      rust-analyzer

      # Plugins
      fishPlugins.tide  # Fish shell theme

      # Qt and UI Libraries
      qt5.full  # Qt5 framework

      # Desktop Environment Tools
      inputs.ags.packages."${system}".default  # For hyprpanel
      hyprpanel  # Panel for Hyprland
      swww  # Wallpaper manager for Wayland
      hyprpicker  # Color picker for Hyprland
      grim  # Screenshot utility for Wayland
      slurp  # Region selection tool for Wayland
      playerctl  # Media player controller
      util-linux 
      
      # GTK themes and tools
      gtk3
      gtk4
      nwg-look  # GTK settings editor
      adwaita-icon-theme
      graphite-gtk-theme

      # Multimedia
      mpv  # Media player
      imv  # Image viewer

      # Compression and Archiving
      unzip  # Extraction utility

      # Browsers and Internet
      inputs.zen-browser.packages."${system}".default  # Browser
      qbittorrent  # Torrent client

      # Game Engines
      godot_4  # Game development engine

      mangohud
      gamescope
    ];
  };

  # ╭────────────────────────────────────────────╮
  # │ Sudo Configuration                         │
  # ╰────────────────────────────────────────────╯
  security.sudo.extraRules= [
    {  
      users = [ "chilly" ];
      commands = [
        { command = "ALL";
          options= [ "NOPASSWD" ];  # SECURITY WARNING: No password required for all commands
        }
      ];
    }
  ];

  # ╭────────────────────────────────────────────────────────────────╮
  # │ System-wide Packages and Environment Configuration             │
  # ╰────────────────────────────────────────────────────────────────╯
  environment = {
    systemPackages = with pkgs; [
      neovim  # Text editor
      curl  # Data transfer tool
      wget  # Web file retriever
      git  # Version control system
      gcc  # GNU Compiler Collection
      lsd  # Modern ls alternative
      wl-clipboard-rs  # Wayland clipboard utility
      file  # File type identification
      llvmPackages_latest.libcxx  # LLVM C++ library
      lua53Packages.jsregexp  # Lua regex library
    ];
  };

  environment.sessionVariables = {
    WLR_NO_HARDWARE_CURSORS = "1";
    NIXOS_OZONE_WL = "1";
  };

  # ╭────────────────────────────────────╮
  # │ Programs Configuration             │
  # ╰────────────────────────────────────╯
  programs.hyprland = {
    enable = true;  # Enable Hyprland Wayland compositor
    xwayland.enable = true;  # Enable XWayland for X11 app support
  };

  # Enable various programs
  programs.fish.enable = true;  # Fish shell
  programs.npm.enable = true;  # Node.js package manager
  programs.adb.enable = true;  # Android Debug Bridge
  programs.nix-ld.enable = true;  # Dynamic linker for nix
  programs.nix-ld.libraries = with pkgs; [
    # Uncomment libraries as needed
    # at-spi2-core
    # boost
    # cairo
    # udisks
    # curl
    # dbus
    # fmt
    # fuse3
    # glib
    # gtk3
    # gtk3-x11
    # openssl
    # pango
    # qt5.qtbase
  ];

  # ╭───────────────────────────╮
  # │ Font Configuration        │
  # ╰───────────────────────────╯
  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono  # Developer font with icons
    nerd-fonts.iosevka  # Slender monospace font with icons
  ];

  # ╭────────────────────────────────────────────╮
  # │ GTK Theme Configuration                    │
  # ╰────────────────────────────────────────────╯
  environment.etc."gtk-3.0/settings.ini" = {
    text = ''
      [Settings]
      gtk-theme-name=Graphite-Dark
    '';
  };

  # ╭────────────────────────────────────────────╮
  # │ Nix Package Manager Configuration          │
  # ╰────────────────────────────────────────────╯
  nix.settings = {
    experimental-features = ["nix-command" "flakes"];  # Enable flakes and nix-command
  };

  # ╭───────────────────────────────────────────────────────╮
  # │ Hardware Configuration                                │
  # ╰───────────────────────────────────────────────────────╯
  hardware.bluetooth.enable = true;  # Enable Bluetooth support
  hardware.bluetooth.powerOnBoot = true;  # Power on Bluetooth at boot
  hardware.graphics.enable = true;  # Enable graphics support

  # ╭──────────────────────────────────────────────╮
  # │ NVIDIA Configuration                         │
  # ╰──────────────────────────────────────────────╯
  hardware.nvidia = {
    modesetting.enable = true;  # Enable kernel modesetting
    powerManagement.enable = true;  # Enable power management
    nvidiaSettings = true;  # Enable nvidia-settings utility
    package = config.boot.kernelPackages.nvidiaPackages.stable;  # Use stable NVIDIA drivers
    open = false;  # Don't use open-source drivers
  };

  hardware.nvidia.prime = {
    nvidiaBusId = "PCI:1:0:0";  # Likely incorrect for your specific hardware
    amdgpuBusId = "PCI:10:0:0";  # Likely incorrect for your specific hardware
  };

  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;

  # ╭────────────────────────────────────────╮
  # │ X Server Configuration                 │
  # ╰────────────────────────────────────────╯
  services.xserver.videoDrivers = ["nvidia"];  # Use NVIDIA drivers for X server

  # ╭─────────────────────────────────────────╮
  # │ Overlays and Package Configuration      │
  # ╰─────────────────────────────────────────╯
  nixpkgs.overlays = [ inputs.hyprpanel.overlay ];  # Add hyprpanel overlay
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "nvidia-x11"
    "nvidia-settings"
  ];
  nixpkgs.config.nvidia.acceptLicense = true;

  # ╭────────────────────────────────────────────────────────────╮
  # │ System State Version                                       │
  # ╰────────────────────────────────────────────────────────────╯
  system.stateVersion = "24.05";  # VERSION WARNING: Might be newer than current stable NixOS version
}
