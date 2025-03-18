set -U fish_greeting
fish_vi_key_bindings

export DISPLAY=:0
export RUST_BACKTRACE=full
export MOZ_ENABLE_WAYLAND=1
export QT_QPA_PLATFORM=wayland
export QT_QPA_PLATFORMTHEME=qt6ct

export XDG_CONFIG_HOME=$HOME/.config
export XDG_CURRENT_DESKTOP=Hyprland
export XDG_SESSION_DESKTOP=Hyprland
export XDG_SESSION_TYPE=wayland
export XCURSOR_SIZE=24

export EDITOR=nvim
export VISUAL=nvim

export FZF_DEFAULT_OPTS="--height=80% --layout=reverse --info=inline --border --margin=1 --padding=1 --wrap --gap=1 --no-separator --pointer=✦ --color=16 --color='gutter:-1,fg+:2,fg:7'"
export HYRCURSOR_THEME="Bibata-Modern_Classic"
export HYRCURSOR_TRACE=1
export HYRCURSOR_SIZE=24;

export WLR_NO_HARDWARE_CURSORS=1
export LIBVA_DRIVER_NAME=nvidia
export __GLX_VENDOR_LIBRARY_NAME=nvidia
export GBM_BACKEND=nvidia-drm
export __GL_GSYNC_ALLOWED=1
export _GL_VRR_ALLOWED=1
export WLR_RENDERER_ALLOW_SOFTWARE=1

fish_add_path path $HOME/Scripts/
# fish_add_path path $HOME/.config/emacs/bin

if status is-interactive
	# Commands to run in interactive sessions can go here
	#  * Create missing directories in path when calling `mkdir`

  if lsd --version > /dev/null
    alias l="lsd -L";
    alias ls="lsd -L";
    alias la="lsd -A";
    alias ll="lsd -lA --date relative --sort git";
    alias lt="lsd --tree";
    alias lr="lsd -R";
  else
    alias la='ls -A'
    alias ll='ls -lA'
  end
  
	alias cp='cp -ir'
	alias mv='cp -i'
	alias mkdir='mkdir -p'
	alias o='~/Scripts/launch'
	alias mv='mv'
	alias fs='df -h -x squashfs -x tmpfs -x devtmpfs'
	
	# Other Simple aliases
	alias n='nvim'
	alias fc='nvim ~/.config/fish/config.fish'
	alias hc='nvim ~/.config/hypr/hyprland.conf'
	alias nc='cd ~/flakes/ && nvim ~/flakes/configuration.nix'
	alias ac='nvim ~/.config/alacritty/alacritty.toml'
  alias f='cd $(fd ~ | fzf)';
  alias o="~/Scripts/launch";
  alias n="nvim";
  alias t="nvim ~/Notes/todo.md";
	alias yay='yay --color=auto'
  alias pyv='python venv ./bin/activate.fish'
	alias ins='yay --color=auto -S'
	alias upd='yay --color=auto -Syu'

  if test -f ~/Codes/SHARKalculator/target/release/rustCalc
    alias calc='~/Codes/SHARKalculator/target/release/rustCalc'
  else
    alias calc='echo "rustCalc not found"'
  end
  
	alias calc='~/Codes/SHARKalculator/target/release/rustCalc'
	alias uni='yay --color=auto -Rcns'
	alias ls='lsd'
	alias nmr='sudo systemctl restart NetworkManager --now'
	alias nm='nmtui'
	alias nr='sudo nixos-rebuild switch --flake ~/flakes'
  alias fs='sudo du -h -d 2 | sort -rh'
	alias la='lsd -A'
	alias lf='lsd --tree --depth=1'
	alias lt='lsd --tree --depth=2'
  alias glpush='cat ~/Documents/gittoken | wl-copy; git push origin $(git branch --show-current)'
  alias ghpush='cat ~/Documents/githubtoken| wl-copy; git push origin $(git branch --show-current)'


  function ni
    nix-search $argv | fzf | awk '{print $1}' | while read -l package;
          nix-env -iA "nixos.$package" || nix-env -iA "nixpkgs.$package" || echo "Package $package not found in nixpkgs or nixos.";
        end
  end


  set fish_cursor_insert line
  set fish_cursor_default block
  set fish_cursor_visual block

  function fish_user_key_bindings
  bind -M insert jk "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"
  end
  
end

zoxide init fish --cmd c| source
