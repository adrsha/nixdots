# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt autocd extendedglob notify
# End of lines configured by zsh-newuser-install
############################################
# Non-interactive shells: exit early
############################################
[[ $- != *i* ]] && return;

############################################
# Vi mode + jk escape
############################################

# Enable vi keymap
bindkey -v

# Faster mode switch
KEYTIMEOUT=10  # smaller = faster; 10 = ~0.1s

# Create a ZLE widget for jk escape
function _jk_escape() {
  local buf="$BUFFER"
  local pos="$CURSOR"

  if [[ $LBUFFER == *j ]]; then
    LBUFFER="${LBUFFER%j}"
    zle vi-cmd-mode
  else
    zle self-insert
  fi
}
zle -N _jk_escape

# Bind 'k' in insert mode to check for 'jk'
bindkey -M viins 'k' _jk_escape

############################################
# General environment (safe for SSH + TTY)
############################################

export DISPLAY=:0
export RUST_BACKTRACE=full
export MOZ_ENABLE_WAYLAND=1
export QT_QPA_PLATFORM=wayland
export QT_QPA_PLATFORMTHEME=qt6ct

export XDG_CONFIG_HOME=$HOME/.config
# export XDG_CURRENT_DESKTOP=Hyprland
export XDG_SESSION_DESKTOP=Hyprland
export XDG_SESSION_TYPE=wayland
export XCURSOR_SIZE=24

export TERM=xterm-ghostty;
export EDITOR=nvim;
export VISUAL=nvim;
export XDG_DATA_HOME="$HOME/.local/share";
export XDG_CONFIG_HOME="$HOME/.config";
export XDG_STATE_HOME="$HOME/.local/state";
export XDG_CACHE_HOME="$HOME/.cache";

# Terminal compatibility (Ghostty / SSH / tmux)
export TERM="${TERM:-xterm-256color}";
export COLORTERM=truecolor;

# Pager
export LESS='-R --use-color -Dd+r$Du+b';

############################################
# fzf (only if available)
############################################
if command -v fzf &>/dev/null; then
    export FZF_DEFAULT_OPTS="\
    --height=70% \
    --layout=reverse \
    --info=inline \
    --no-border \
    --no-separator \
    --margin 0 \
    --wrap \
    --pointer=✦ \
    --color='fg:15,fg+:16,bg+:3,pointer:3,marker:3,info:15,prompt:15' \
    --bind='ctrl-l:accept'"
fi

############################################
# PATH (defensive: prevent duplicates)
############################################
__add_to_path() {
    local dir="$1";
    [[ -d "$dir" && ":$PATH:" != *":$dir:"* ]] && PATH="$dir:$PATH";
};

__add_to_path "$HOME/.local/bin";
__add_to_path "$HOME/Scripts";
export PATH;

############################################
# Aliases
############################################
alias cp='cp -ir';
alias mv='mv -i';
alias rm='trash -v';
alias mkdir='mkdir -p';

alias ..='cd ..';
alias ...='cd ../..';
alias ....='cd ../../..';

alias n='nvim';
# alias bc='nvim ~/.bashrc';
alias zc='nvim ~/.zshrc';
alias hc='nvim ~/.config/hypr/hyprland.conf';

alias stoprecord='pkill -SIGINT -f gpu-screen-recorder';
alias startrecord='gpu-screen-recorder -w portal -f 60 -a default_output -o ~/Videos/$(date -d now +%F_%H-%M).mkv';
alias calc='~/Codes/misc/rumat/target/release/rustCalc';
alias yay='yay --color=auto --bottomup';
alias ins='yay --color=auto -S';
alias upd='yay --color=auto -Syu';
alias uni='sudo pacman -Rcns $(pacman -Qeq | fzf)';
alias duni='sudo pacman -Rcns $(pacman -Qq | fzf)';
alias gclone='cat ~/Documents/githubtoken | wl-copy && git clone';
alias ghpush='cat ~/Documents/githubtoken | wl-copy && git push';

alias nm='nmtui';
alias nmr='sudo systemctl restart NetworkManager --now';

############################################
# ls (lsd if available)
############################################
if command -v lsd &>/dev/null; then
    alias l='lsd -lA --date relative --sort git';
    alias ls='lsd -L';
    alias la='lsd -A';
    alias ll='lsd -lA --date relative --sort git';
    alias lt='lsd --tree --depth 3';
else
    alias ls='ls --color=auto';
    alias la='ls -A';
    alias ll='ls -lA';
fi;

############################################
# Useful functions
############################################
tk() {
    [[ -z "$1" ]] && { echo "Usage: tk <directory>"; return 1; };
    mkdir -p "$1" && cd "$1" || return 1;
};

whatis() {
    [[ -z "$*" ]] && { echo "Usage: whatis <query>"; return 1; };
    local url="https://cheat.sh/$*";
    
    if command -v bat &>/dev/null; then
        curl -fsSL "$url" 2>/dev/null | bat || echo "Failed to fetch cheat sheet";
    else
        curl -fsSL "$url" 2>/dev/null || echo "Failed to fetch cheat sheet";
    fi;
};



# Use a line cursor
echo -ne '\e[6 q';
autoload -U compinit; compinit

source ~/powerlevel10k/powerlevel10k.zsh-theme
source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source ~/.config/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
source ~/.config/zsh/fzf-tab/fzf-tab.plugin.zsh

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


############################################
# zoxide (optional)
############################################
if command -v zoxide &>/dev/null; then
    eval "$(zoxide init zsh --cmd c)";
fi;

