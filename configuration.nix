{ config, pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  environment.sessionVariables = {
    TERM = "xterm-256color";
    DISPLAY = ":0";
    RUST_BACKTRACE = "full";

    XDG_SESSION_TYPE = "wayland";
    XDG_SESSION_DESKTOP = "Hyprland";
    XDG_CURRENT_DESKTOP = "Hyprland";
    HYPRLAND_TRACE = "1";

    # Nvidia Optimizations
    # WLR_NO_HARDWARE_CURSORS = "1";
    # LIBVA_DRIVER_NAME = "nvidia";
    # GBM_BACKEND = "nvidia-drm";
    # __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    # __GL_GSYNC_ALLOWED = "1";
    # __GL_VRR_ALLOWED = "0";
    # __NV_PRIME_RENDER_OFFLOAD = "1";
    # NVIDIA_FORCE_PROBE = "1";
    # NVD_BACKEND = "direct";

    # Rendering
    # WLR_BACKEND = "vulkan";
    # WLR_RENDERER_ALLOW_SOFTWARE = "1";
    # WLR_DRM_NO_ATOMIC = "1";
    # WLR_USE_LIBINPUT = "1";
    # WLR_DRM_DEVICES = "/dev/dri/card1:/dev/dri/card0";

    MOZ_ENABLE_WAYLAND = "1";
    EDITOR = "nvim";
    VISUAL = "nvim";

    FZF_DEFAULT_OPTS = "--height=80% --layout=reverse --info=inline --border --margin=1 --padding=1 --wrap --gap=1 --no-separator --pointer=✦ --color=16 --color='gutter:-1,fg+:2,fg:7'";
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.extraModulePackages = [
    config.boot.kernelPackages.msi-ec
    config.boot.kernelPackages.nvidia_x11
  ];

  boot.kernelModules = [
    "msi-ec"
    "kvm-amd"
    "nvidia"
  ];

  boot.kernelParams = [
    "nvidia_drm.modeset=1"
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
  ];

  networking.hostName = "nixos"; # Sets the hostname of the system
  networking.nameservers = [
    "8.8.8.8"
    "1.1.1.1"
  ];
  services.resolved.enable = false;
  networking.networkmanager.enable = true;

  time.timeZone = "Asia/Kathmandu";

  services.xserver.videoDrivers = [ "nvidia" ]; # Use NVIDIA drivers for X server
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true; # For 32-bit application support
    pulse.enable = true;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.adwaita-mono
    nerd-fonts.jetbrains-mono
    nerd-fonts.martian-mono
    nerd-fonts.iosevka-term
  ];

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };
  programs.kdeconnect.enable = true;
  programs.fish.enable = true;
  programs.npm.enable = true;
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ ];

  users.users.chilly = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "kvm"
      "libvertd"
    ];
    shell = pkgs.fish;

    packages = with pkgs; [
      ghostty

      # terminal commands
      bat
      bluetui
      fzf
      igrep
      killall
      lsd
      onefetch
      ripgrep
      tdf
      tldr
      zoxide

      # shell plugins
      fishPlugins.tide

      # desktop utils
      brightnessctl
      firefox
      grim
      hyprpicker
      slurp
      wl-clipboard-rs
      nwg-look

      # Themes
      amarena-theme
      whitesur-icon-theme
      nordzy-icon-theme

      # Multimedia
      imv
      mpv
      playerctl
      awww
      yt-dlp

      # build systems
      cmake
      ninja

      # languages
      rustup

      # language servers
      astro-language-server
      bash-language-server
      cpplint
      jq-lsp
      lua-language-server
      nil
      rust-analyzer
      rustc
      typescript-language-server
      vscode-langservers-extracted

    ];
  };

  security.sudo.enable = true;
  security.sudo.extraRules = [
    {
      users = [ "chilly" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

  environment.systemPackages = with pkgs; [
    neovim
    git
    curl
    wget
    gcc
  ];

  environment.etc."gtk-3.0/settings.ini" = {
    text = ''
      [Settings]
      gtk-theme-name=Graphite-Dark
    '';
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  hardware.graphics.enable = true;

  hardware.nvidia = {
    modesetting.enable = true; # Enable kernel modesetting
    powerManagement.finegrained = false;
    powerManagement.enable = false; # Enable power management
    nvidiaSettings = true; # Enable nvidia-settings utility
    open = false; # Don't use open-source drivers
    videoAcceleration = true;
    package = config.boot.kernelPackages.nvidiaPackages.production; # Use stable NVIDIA drivers
    # forceFullCompositionPipeline = true;
  };

  hardware.nvidia-container-toolkit.enable = true;
  hardware.cpu.amd.updateMicrocode = true;
  hardware.nvidia.gsp.enable = true;
  hardware.enableRedistributableFirmware = true;

  # VERSION WARNING: Might be newer than current stable NixOS version
  system.stateVersion = "25.11";
}
