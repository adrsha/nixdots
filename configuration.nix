# ╭───────────────────────────────────────────────────────────────────────────────╮
# │                       Main NixOS Configuration File                           │
# │   Defines system configuration, specifying what to install and configure.     │
# ╰───────────────────────────────────────────────────────────────────────────────╯

{
    config,
        lib,
        pkgs,
        inputs,
        ...
}:

{

# ╭─────────────────────────────╮
# │ Boot Configuration          │
# ╰─────────────────────────────╯
    boot.loader.systemd-boot.enable = true; # Uses systemd-boot as the bootloader
        boot.loader.efi.canTouchEfiVariables = true; # Allow modifying EFI variables
        boot.initrd.luks.devices.cryptroot.device =
        "/dev/disk/by-uuid/25f8aed9-c1b3-4bba-952e-8c17e7ba74d9";

# ╭────────────────────────────────────────────────────────╮
# │ MSI utils                                              │
# ╰────────────────────────────────────────────────────────╯
    boot.extraModulePackages = [
        config.boot.kernelPackages.msi-ec
            config.boot.kernelPackages.nvidia_x11
    ];
    boot.kernelModules = [
        "msi-ec"
            "kvm-amd"
            "nvidia"
    ];
# ╭────────────────────────────────────────────────────────╮
# │ Kernel Parameters                                      │
# ╰────────────────────────────────────────────────────────╯
    boot.kernelParams = [
        "nvidia_drm.modeset=1"
            "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
    ];

# ╭──────────────────────────────╮
# │ Network Configuration        │
# ╰──────────────────────────────╯
    networking.hostName = "nixos"; # Sets the hostname of the system
        networking.nameservers = [
        "8.8.8.8"
            "1.1.1.1"
        ];
    services.resolved.enable = false;

    networking.networkmanager.enable = true; # Enables NetworkManager for network management
        time.timeZone = "Asia/Kathmandu";

# ╭────────────────────────────────────╮
# │ Firewall Configuration             │
# ╰────────────────────────────────────╯
    networking.firewall = {
        enable = false;
        allowedTCPPorts = [
            80
                443
        ]; # Common web ports
        allowedUDPPorts = [ 53 ]; # DNS
    };

# ╭──────────────────────────────────────────────────────────╮
# │ Audio Configuration                                      │
# ╰──────────────────────────────────────────────────────────╯
    services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true; # For 32-bit application support
            pulse.enable = true;
    };
# ╭──────────────────────────────────────────────────────────────────╮
# │ User Configuration                                               │
# ╰──────────────────────────────────────────────────────────────────╯
    users.users.chilly = {
        isNormalUser = true;
        initialPassword = "changeme";
        extraGroups = [
            "wheel"
                "kvm"
                "libvertd"
        ]; # Enable 'sudo' for the user
        shell = pkgs.fish; # Set the default shell to fish
        packages = with pkgs; [

######################################
#         SYSTEM UTILITIES           #
######################################

        brightnessctl # Brightness control utility
            efibootmgr # EFI boot manager
            gnumake # GNU Make
            gnupg # GNU Privacy Guard
            killall # Process termination utility
            libnotify # Desktop notifications
            util-linux # Miscellaneous essential utilities
            xorg.xset # X server settings utility
            xorg.xhost
            xorg.xrandr
            xorg.libxcvt
            payload-dumper-go
            appimage-run
            imagemagick
            gnome-sound-recorder
            pulseaudio
            oterm

######################################
#     SYSTEM MONITORING & STATS      #
######################################

            btop # Resource monitor
            archimede
            onefetch # Git repo summary
            microfetch

######################################
#         TERMINAL TOOLS             #
######################################

            fzf # Fuzzy finder
            bat # Modern cat with syntax highlighting
            ripgrep # Fast grep alternative
            zoxide # Smarter cd command
            lsd # Modern ls alternative
            ueberzugpp # Terminal image preview
            clipse # Clipboard manager
            wev # Wayland event viewer
            tmux
            ydotool # Input emulation for Wayland
            nh
            wezterm
            sshfs
            wiper
            bluetui
            binsider
            glow
            tdf
            igrep
            repgrep
            netscanner
            mapscii
            tray-tui
            browsh
            hwatch
            serie
            tldr

######################################
#          DEVELOPMENT TOOLS         #
######################################
            emacs-pgtk
            cmake # Build system
            ninja # Build system backend
            python3 # Python interpreter
            graphviz
            uv
            zed-editor # Zed Code editor
            ollama-cuda # LLM CLI with CUDA

######################################
#        PROGRAMMING LANGUAGES       #
######################################

# flutter               # UI toolkit for app development
            typescript # JavaScript superset

######################################
#   LINTING & LANGUAGE SERVERS (LSP) #
######################################

            astro-language-server
            bash-language-server
            cpplint # C++ linter
            jq-lsp # JSON language server
            lua-language-server
            nixd # Nix language server
            nil# Nix language server
            nodePackages.prettier # Code formatter
            pyright # Python language server
            rust-analyzer # Rust language server
            rustc
            rustup
            typescript-language-server
            vscode-langservers-extracted
            qpwgraph

######################################
#        SHELL PLUGINS / THEMES      #
######################################

            fishPlugins.tide # Fish shell theme

######################################
#              GUI TOOLKITS          #
######################################

# qt5.full              # Qt5 libraries
            gtk3 # GTK 3 UI toolkit
            gtk4 # GTK 4 UI toolkit

######################################
#            GTK CUSTOMIZATION       #
######################################

            nwg-look # GTK appearance editor
            adwaita-icon-theme # Default GNOME icon theme
            graphite-gtk-theme # Custom GTK theme

######################################
#     WAYLAND / DESKTOP COMPONENTS   #
######################################

            mako
            waybar
            quickshell
            walker
            grim # Screenshot tool
            slurp # Selection tool for grim
            hyprpicker # Color picker
            xfce.thunar # File manager
            blueberry # Bluetooth GUI

######################################
#             MULTIMEDIA             #
######################################

            yt-dlp # Media downloader (dev utility use case)
            playerctl # Media controller
            swww # Wayland wallpaper daemon
            mpv # Media player
            imv # Lightweight image viewer
            telegram-desktop
            libreoffice-qt6-fresh

######################################
#             Utils                  #
######################################

            emote
            matugen # Material color generator

######################################
#     COMPRESSION / ARCHIVING TOOLS  #
######################################

            ouch # Zip creation utility

######################################
#         INTERNET & BROWSERS        #
######################################

# inputs.zen-browser.packages."${system}".default

            firefox
            vivaldi
            qbittorrent # BitTorrent client
            discord

######################################
#            BIG MISC APPS            #
######################################

            godot_4 # Game engine
# inputs.prism.packages."${system}".default

######################################
#             GPU TOOLS              #
######################################

            cudatoolkit # CUDA libraries
            mesa # Graphics drivers
            linux-firmware # Firmware blobs

            ];
    };

# ╭────────────────────────────────────────────╮
# │ Sudo Configuration                         │
# ╰────────────────────────────────────────────╯
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

# ╭────────────────────────────────────────────────────────────────╮
# │ System-wide Packages and Environment Configuration             │
# ╰────────────────────────────────────────────────────────────────╯
    environment = {
        systemPackages = with pkgs; [
            neovim # Text editor
                curl # Data transfer tool
                wget # Web file retriever
                git # Version control system
                gcc # GNU Compiler Collection
                lsd # Modern ls alternative
                wl-clipboard-rs # Wayland clipboard utility
                file # File type identification
                llvmPackages_latest.libcxx # LLVM C++ library
                lua53Packages.jsregexp # Lua regex library
                sbctl # for lanzaboote/secure boot
                niv # for lanzaboote/secure boot
                wlr-randr
# pkg-config
# openssl
        ];
    };

#=================================================
# ENVIRONMENT VARIABLES
#=================================================
    environment.sessionVariables = {
# General Settings
        TERM = "xterm-256color";
        DISPLAY = ":0";
        RUST_BACKTRACE = "full";

# Wayland & Hyprland
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
# Editor & Shell
        EDITOR = "nvim";
        VISUAL = "nvim";

# Ollama (AI Acceleration)
# OLLAMA_ACCELERATE = "1";
# OLLAMA_BACKEND = "cuda";

# FZF Customization
        FZF_DEFAULT_OPTS = "--height=80% --layout=reverse --info=inline --border --margin=1 --padding=1 --wrap --gap=1 --no-separator --pointer=✦ --color=16 --color='gutter:-1,fg+:2,fg:7'";

# RUST ENV VARS
# RUSTC_LINKER    = "${pkgs.gcc}/bin/gcc";

# Paths
        XDG_CONFIG_HOME = "$HOME/.config";
    };

# ╭────────────────────────────────────╮
# │ Programs Configuration             │
# ╰────────────────────────────────────╯
    programs.hyprland = {
        enable = true; # Enable Hyprland Wayland compositor
            xwayland.enable = true; # Enable XWayland for X11 app support
    };
    programs.kdeconnect.enable = true;
    programs.gamemode.enable = true;

# Enable various programs
    programs.fish.enable = true; # Fish shell
        programs.npm.enable = true; # Node.js package manager
        programs.adb.enable = true; # Android Debug Bridge
        programs.nix-ld.enable = true; # Dynamic linker for nix
        programs.nix-ld.libraries = with pkgs; [
# Uncomment libraries as needed
# stdenv.cc.cc.lib
# zlib
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
        nerd-fonts.adwaita-mono # Developer font with icons
            nerd-fonts.jetbrains-mono # Developer font with icons
            nerd-fonts.martian-mono # Developer font with icons
            nerd-fonts.iosevka-term # Slender monospace font with icons
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
        experimental-features = [
            "nix-command"
                "flakes"
        ]; # Enable flakes and nix-command
        trusted-users = [
        "root"
            "chilly"
        ];
    };

# ╭───────────────────────────────────────────────────────╮
# │ Hardware Configuration                                │
# ╰───────────────────────────────────────────────────────╯
    hardware.bluetooth.enable = true; # Enable Bluetooth support
        hardware.bluetooth.powerOnBoot = true; # Power on Bluetooth at boot
        hardware.graphics.enable = true; # Enable graphics support

# ╭──────────────────────────────────────────────╮
# │ NVIDIA Configuration                         │
# ╰──────────────────────────────────────────────╯
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
    services.xserver.videoDrivers = [ "nvidia" ]; # Use NVIDIA drivers for X server

        services.tor = {
            enable = false;
            openFirewall = true;
            relay = {
                enable = true;
                role = "relay";
            };
            settings = {
                ContactInfo = "toradmin@example.org";
                Nickname = "toradmin";
                ORPort = 9001;
                ControlPort = 9051;
                BandWidthRate = "1 MBytes";
                DNSPort = [{
                    addr = "127.0.0.1";
                    port = 53;
                }];
            };
        };
    services.ollama.acceleration = [ "cuda" ];
    hardware.nvidia-container-toolkit.enable = true;
    hardware.nvidia.prime = {
        offload = {
            enable = true;
            enableOffloadCmd = true;
        };

        nvidiaBusId = "PCI:1:0:0"; # Likely incorrect for your specific hardware
            amdgpuBusId = "PCI:10:0:0"; # Likely incorrect for your specific hardware
    };
    hardware.cpu.amd.updateMicrocode = true;
    hardware.nvidia.gsp.enable = true;
    hardware.enableRedistributableFirmware = true;

# ╭────────────────────────────────────────╮
# │ X Server Configuration                 │
# ╰────────────────────────────────────────╯

# ╭─────────────────────────────────────────╮
# │ Overlays and Package Configuration      │
# ╰─────────────────────────────────────────╯
    nixpkgs.config.allowUnfree = true;
# nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
#   "nvidia-x11"
#   "nvidia-settings"
#   "vscode"
#   "anydesk"
# ];
    nixpkgs.config.nvidia.acceptLicense = true;

# ╭────────────────────────────────────────────────────────────╮
# │ System State Version                                       │
# ╰────────────────────────────────────────────────────────────╯
    system.stateVersion = "24.05"; # VERSION WARNING: Might be newer than current stable NixOS version
}
