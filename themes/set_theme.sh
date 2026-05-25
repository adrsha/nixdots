#!/usr/bin/env sh
# set_theme.sh — load a theme file, export THEME_XXX vars,
# and generate config files for ghostty, hyprland, vicinae, and swaync.
#
# Usage:
#   . set_theme.sh ~/dots/themes/aquarium
#
# Or define an alias:  alias theme='. ~/path/to/set_theme.sh'

if [ $# -eq 0 ]; then
    echo "set_theme.sh: missing argument." >&2
    echo "  usage: . set_theme.sh <theme_file>" >&2
    return 1
fi

theme_file="$1"

if [ ! -f "$theme_file" ]; then
    echo "set_theme.sh: file not found: $theme_file" >&2
    return 1
fi

# ---------------------------------------------------------------------------
# Change detection — skip everything if the theme file hasn't changed.
# ---------------------------------------------------------------------------
new_hash=$(sha256sum "$theme_file" | cut -d' ' -f1)

_runtime_dir="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
_hash_file="$_runtime_dir/set_theme.hash"
_vars_file="$_runtime_dir/set_theme.vars"

_old_hash=""
[ -f "$_hash_file" ] && _old_hash=$(cat "$_hash_file")

if [ "$new_hash" = "$_old_hash" ]; then
    return 0
fi

# ---------------------------------------------------------------------------
# Erase any previously exported THEME_ vars so stale keys don't linger.
# ---------------------------------------------------------------------------
if [ -f "$_vars_file" ]; then
    while IFS= read -r _var; do
        unset "$_var"
    done < "$_vars_file"
fi

# ---------------------------------------------------------------------------
# Helper: look up a key's value in the theme file.
# ---------------------------------------------------------------------------
get_theme_val() {
    _result=$(grep "^${1}=" "$theme_file" | head -1 | cut -d= -f2-)
    if [ -z "$_result" ]; then
        echo "set_theme.sh: theme key not found: $1" >&2
        return 1
    fi
    echo "$_result"
}

# ---------------------------------------------------------------------------
# Helper: convert #RRGGBB to "R, G, B".
# ---------------------------------------------------------------------------
hex_to_rgb() {
    _h="${1#\#}"
    _r=$(printf '%d' "0x$(printf '%s' "$_h" | cut -c1-2)")
    _g=$(printf '%d' "0x$(printf '%s' "$_h" | cut -c3-4)")
    _b=$(printf '%d' "0x$(printf '%s' "$_h" | cut -c5-6)")
    printf '%d, %d, %d' "$_r" "$_g" "$_b"
}

# ---------------------------------------------------------------------------
# Read the theme file: export THEME_<KEY>=<val> for every key/value pair.
# ---------------------------------------------------------------------------
THEME_PATH=$(realpath "$theme_file")
THEME_NAME=$(basename "$theme_file")
export THEME_PATH THEME_NAME

while IFS= read -r _line; do
    case "$_line" in
        ''|'#'*) continue ;;
    esac

    _key=$(printf '%s' "$_line" | cut -d= -f1 | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')
    _val=$(printf '%s' "$_line" | cut -d= -f2-)

    [ -z "$_key" ] && continue

    export "THEME_${_key}=${_val}"
done < "$theme_file"

# Validate VARIANT was declared in the theme file.
case "$THEME_VARIANT" in
    dark|light) ;;
    *)
        echo "set_theme.sh: VARIANT must be 'dark' or 'light' in $theme_file" >&2
        return 1
        ;;
esac

# Record exported THEME_ var names for cleanup on the next run.
env | grep '^THEME_' | cut -d= -f1 > "$_vars_file"

# ---------------------------------------------------------------------------
# Write a lua env-patch file so running nvim instances can update their
# stale vim.env before rebuilding the theme.
# Written before the socket loop so it's ready when reload_nvchad_theme runs.
# ---------------------------------------------------------------------------
nvim_patch_file=/tmp/nvim_theme_patch.lua

{
    printf 'vim.env.THEME_NAME    = "%s"\n' "$THEME_NAME"
    printf 'vim.env.THEME_VARIANT = "%s"\n' "$THEME_VARIANT"

    while IFS= read -r _line; do
        case "$_line" in
            ''|'#'*) continue ;;
        esac
        _key=$(printf '%s' "$_line" | cut -d= -f1 | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')
        _val=$(printf '%s' "$_line" | cut -d= -f2-)
        [ -z "$_key" ] && continue
        printf 'vim.env["THEME_%s"] = "%s"\n' "$_key" "$_val"
    done < "$theme_file"
} > "$nvim_patch_file"

# ---------------------------------------------------------------------------
# Ghostty
# ---------------------------------------------------------------------------
g_base00=$(get_theme_val BASE00)
g_base01=$(get_theme_val BASE01)
g_base02=$(get_theme_val BASE02)
g_base05=$(get_theme_val BASE05)
g_base07=$(get_theme_val BASE07)
g_base08=$(get_theme_val BASE08)
g_base09=$(get_theme_val BASE09)
g_base0a=$(get_theme_val BASE0A)
g_base0b=$(get_theme_val BASE0B)
g_base0c=$(get_theme_val BASE0C)
g_base0d=$(get_theme_val BASE0D)
g_base0e=$(get_theme_val BASE0E)
g_base0f=$(get_theme_val BASE0F)

{
    printf 'palette = 0=%s\n'  "$g_base00"
    printf 'palette = 1=%s\n'  "$g_base08"
    printf 'palette = 2=%s\n'  "$g_base0b"
    printf 'palette = 3=%s\n'  "$g_base0a"
    printf 'palette = 4=%s\n'  "$g_base0d"
    printf 'palette = 5=%s\n'  "$g_base0e"
    printf 'palette = 6=%s\n'  "$g_base0c"
    printf 'palette = 7=%s\n'  "$g_base05"
    printf 'palette = 8=%s\n'  "$g_base01"
    printf 'palette = 9=%s\n'  "$g_base08"
    printf 'palette = 10=%s\n' "$g_base0b"
    printf 'palette = 11=%s\n' "$g_base0a"
    printf 'palette = 12=%s\n' "$g_base0d"
    printf 'palette = 13=%s\n' "$g_base0e"
    printf 'palette = 14=%s\n' "$g_base0c"
    printf 'palette = 15=%s\n' "$g_base07"
    printf 'background           = %s\n' "$g_base00"
    printf 'foreground           = %s\n' "$g_base05"
    printf 'cursor-color         = %s\n' "$g_base05"
    printf 'selection-background = %s\n' "$g_base02"
    printf 'selection-foreground = %s\n' "$g_base05"
} > ~/dots/ghostty/themes/active

# ---------------------------------------------------------------------------
# Hyprland
# ---------------------------------------------------------------------------
h_border=$(get_theme_val BASE02      | sed 's/^#//')
h_background=$(get_theme_val DARKER_BLACK | sed 's/^#//')

cat > ~/dots/hypr/colors.conf << EOF
general {
    col.active_border   = rgb($h_border)
    col.inactive_border = rgb($h_border)
}
misc {
    background_color = rgb($h_background)
}
EOF

# ---------------------------------------------------------------------------
# Vicinae
# ---------------------------------------------------------------------------
v_name=$THEME_NAME
v_accent=$(get_theme_val RED)
v_bg=$(get_theme_val BLACK)
v_fg=$(get_theme_val WHITE)
v_darker_black=$(get_theme_val DARKER_BLACK)
v_base01=$(get_theme_val BASE01)
v_base03=$(get_theme_val BASE03)
v_grey=$(get_theme_val GREY)
v_green=$(get_theme_val GREEN)
v_red=$(get_theme_val RED)
v_yellow=$(get_theme_val YELLOW)
v_teal=$(get_theme_val TEAL)
v_blue=$(get_theme_val BLUE)
v_pink=$(get_theme_val PINK)
v_baby_pink=$(get_theme_val BABY_PINK)
v_purple=$(get_theme_val PURPLE)

cat > ~/dots/vicinae-themes/"$v_name".toml << EOF
[meta]
name        = "$v_name"
description = "Generated by set_theme.sh"
variant     = "dark"
inherits    = "vicinae-dark"

[colors.core]
accent               = "$v_accent"
accent_foreground    = "$v_darker_black"
background           = "$v_bg"
foreground           = "$v_fg"
secondary_background = "$v_bg"
border               = "colors.core.background"

[colors.main_window]
border = "colors.core.background"
footer = { background = "colors.core.secondary_background" }

[colors.settings_window]
border = "colors.core.border"

[colors.accents]
blue    = "$v_teal"
green   = "$v_green"
magenta = "$v_baby_pink"
orange  = "$v_pink"
red     = "$v_red"
yellow  = "$v_yellow"
cyan    = "$v_teal"
purple  = "$v_purple"

[colors.shortcut]
border = "colors.core.border"

[colors.text]
default     = "$v_fg"
muted       = "$v_base03"
danger      = "$v_red"
success     = "$v_green"
placeholder = "$v_base03"
selection   = { background = "$v_grey", foreground = "$v_fg" }

[colors.text.links]
default = "$v_teal"
visited = "$v_blue"

[colors.input]
border       = "$v_bg"
border_focus = "$v_teal"
border_error = "$v_red"

[colors.button.primary]
background = "$v_darker_black"
foreground = "$v_fg"
hover      = { background = "$v_darker_black" }
focus      = { outline = "colors.core.accent" }

[colors.list.item.hover]
foreground           = "$v_fg"
secondary_foreground = "$v_base03"

[colors.list.item.selection]
background           = "$v_base01"
foreground           = "$v_fg"
secondary_background = "$v_bg"
secondary_foreground = "$v_base03"

[colors.grid.item]
background = "$v_bg"
hover      = { outline = "$v_fg" }
selection  = { outline = "$v_fg" }

[colors.scrollbars]
background = "$v_bg"

[colors.loading]
bar     = "$v_teal"
spinner = "$v_teal"
EOF
if [ $? -eq 0 ]; then
    echo "set_theme.sh: wrote ~/dots/vicinae-themes/$v_name.toml"
else
    echo "set_theme.sh: failed to write vicinae config" >&2
fi

# ---------------------------------------------------------------------------
# swaync
# ---------------------------------------------------------------------------
sw_base00=$(get_theme_val BASE00)
sw_one_bg=$(get_theme_val ONE_BG)
sw_one_bg2=$(get_theme_val ONE_BG2)
sw_one_bg3=$(get_theme_val ONE_BG3)
sw_black2=$(get_theme_val BLACK2)
sw_darker_black=$(get_theme_val DARKER_BLACK)
sw_cyan=$(get_theme_val CYAN)
sw_white=$(get_theme_val WHITE)
sw_base03=$(get_theme_val BASE03)
sw_blue=$(get_theme_val BLUE)

sw_base00_rgb=$(hex_to_rgb "$sw_base00")
sw_one_bg_rgb=$(hex_to_rgb "$sw_one_bg")
sw_one_bg2_rgb=$(hex_to_rgb "$sw_one_bg2")
sw_one_bg3_rgb=$(hex_to_rgb "$sw_one_bg3")
sw_black2_rgb=$(hex_to_rgb "$sw_black2")
sw_darker_black_rgb=$(hex_to_rgb "$sw_darker_black")
sw_cyan_rgb=$(hex_to_rgb "$sw_cyan")
sw_white_rgb=$(hex_to_rgb "$sw_white")
sw_base03_rgb=$(hex_to_rgb "$sw_base03")
sw_blue_rgb=$(hex_to_rgb "$sw_blue")

cat > /tmp/swaync_vars.css << EOF
:root {
  --cc-bg:                      rgba($sw_base00_rgb, 1);

  --noti-border-color:          rgba($sw_cyan_rgb, 0.15);

  --noti-bg:                    $sw_one_bg_rgb;
  --noti-bg-alpha:              1;
  --noti-bg-darker:             rgb($sw_darker_black_rgb);
  --noti-bg-hover:              rgb($sw_one_bg2_rgb);
  --noti-bg-focus:              rgba($sw_cyan_rgb, 0.2);

  --noti-close-bg:              rgb($sw_black2_rgb);
  --noti-close-bg-hover:        rgb($sw_one_bg3_rgb);

  --text-color:                 rgb($sw_white_rgb);
  --text-color-disabled:        rgb($sw_base03_rgb);

  --bg-selected:                rgb($sw_blue_rgb);

  --notification-icon-size:       64px;
  --notification-app-icon-size:   calc(var(--notification-icon-size) / 3);
  --notification-group-icon-size: 32px;

  --border:                     0px solid var(--noti-border-color);
  --border-radius:              12px;

  --notification-shadow:
    0 0 0 1px rgba(0, 0, 0, 0.45),
    0 2px 6px rgba(0, 0, 0, 0.65);

  --font-size-body:             15px;
  --font-size-summary:          16px;

  --hover-transition:           background 0.15s ease-in-out;
  --group-collapse-transition:  opacity 400ms ease-in-out;
}

/* GTK fallback */
@define-color cc-bg                  rgba($sw_base00_rgb, 1);
@define-color noti-border-color      rgba($sw_cyan_rgb, 0.15);

@define-color noti-bg                rgba($sw_one_bg_rgb, 1);
@define-color noti-bg-opaque         rgb($sw_one_bg_rgb);
@define-color noti-bg-darker         rgb($sw_darker_black_rgb);

@define-color noti-bg-hover          rgb($sw_one_bg2_rgb);
@define-color noti-bg-hover-opaque   rgb($sw_one_bg2_rgb);

@define-color noti-bg-focus          rgba($sw_cyan_rgb, 0.2);

@define-color noti-close-bg          rgba($sw_black2_rgb, 1);
@define-color noti-close-bg-hover    rgba($sw_one_bg3_rgb, 1);

@define-color text-color             rgb($sw_white_rgb);
@define-color text-color-disabled    rgb($sw_base03_rgb);

@define-color bg-selected            rgb($sw_blue_rgb);

EOF

if [ ! -f ~/dots/swaync/styles_rules.css ]; then
    echo "set_theme.sh: ~/dots/swaync/styles_rules.css not found — swaync style.css not written" >&2
else
    cat /tmp/swaync_vars.css ~/dots/swaync/styles_rules.css > ~/dots/swaync/style.css
    if [ $? -eq 0 ]; then
        echo "set_theme.sh: wrote ~/dots/swaync/style.css"
    else
        echo "set_theme.sh: failed to write swaync config" >&2
    fi

    if command -v swaync-client > /dev/null 2>&1 && pgrep -x swaync > /dev/null; then
        swaync-client -rs \
            && echo "set_theme.sh: reloaded swaync" \
            || echo "set_theme.sh: swaync reload failed" >&2
    fi
fi

# ---------------------------------------------------------------------------
# Reload running nvim instances via their sockets.
# The patch file written above is dofile'd inside reload_nvchad_theme()
# to update stale vim.env before the theme is rebuilt.
# ---------------------------------------------------------------------------
for socket in "$_runtime_dir"/nvim.*/0; do
    [ -S "$socket" ]                          || continue
    [ "$socket" = "$NVIM_LISTEN_ADDRESS" ]    && continue

    nvim --server "$socket" --remote-expr 'v:lua.reload_nvchad_theme()' > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        echo "set_theme.sh: reloaded nvim at $socket"
    else
        echo "set_theme.sh: could not reload nvim at $socket" >&2
    fi
done

# Recompile the base46 highlight cache so the next nvim launch is correct.
nvim --headless -c 'lua require("base46").compile()' -c 'qa' > /dev/null 2>&1
if [ $? -eq 0 ]; then
    echo "set_theme.sh: recompiled base46 cache"
else
    echo "set_theme.sh: base46 cache recompile failed" >&2
fi

# ---------------------------------------------------------------------------
# Persist the hash now that everything succeeded.
# ---------------------------------------------------------------------------
echo "$new_hash" > "$_hash_file"

echo "set_theme.sh: loaded '$THEME_NAME'"
