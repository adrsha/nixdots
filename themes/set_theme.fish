#!/usr/bin/env fish
# set_theme.fish — load a theme file, set THEME_XXX universal env vars,
# and generate config files for ghostty, hyprland, vicinae, and swaync
# from the same source.
#
# Usage:
#   source set_theme.fish ~/dots/themes/aquarium
#
# Or use the alias:  theme ~/dots/themes/aquarium

if test (count $argv) -eq 0
    echo "set_theme.fish: missing argument." >&2
    echo "  usage: source set_theme.fish <theme_file>" >&2
    return 1
end

set theme_file $argv[1]

if not test -f $theme_file
    echo "set_theme.fish: file not found: $theme_file" >&2
    return 1
end

# ---------------------------------------------------------------------------
# Change detection — skip everything if the theme file hasn't changed.
# ---------------------------------------------------------------------------
set new_hash (sha256sum $theme_file | string split ' ')[1]

if test "$new_hash" = "$_theme_hash"
    return 0
end

# Erase any previously set THEME_ vars so stale keys don't linger.
for var in (set --names | string match --regex '^THEME_')
    set --erase --universal $var
    set --erase $var
end

set --universal THEME_PATH (realpath $theme_file)
set --universal THEME_NAME (basename $theme_file)

set --global theme_keys
set --global theme_vals

for line in (cat $theme_file)
    if test -z "$line"; or string match --quiet '#*' $line
        continue
    end

    set key (string replace --regex '=.*' '' $line | string trim)
    set val (string replace --regex '^[^=]*=' '' $line)

    if test -z "$key"
        continue
    end

    set --global --append theme_keys $key
    set --global --append theme_vals $val
    set --universal        "THEME_$key" $val
    set --global --export  "THEME_$key" $val
end

# Validate VARIANT was declared in the theme file.
if not contains -- $THEME_VARIANT dark light
    echo "set_theme.fish: VARIANT must be 'dark' or 'light' in $theme_file" >&2
    return 1
end

function get_theme_val --argument-names wanted_key
    for i in (seq (count $theme_keys))
        if test "$theme_keys[$i]" = $wanted_key
            echo $theme_vals[$i]
            return 0
        end
    end
    echo "set_theme.fish: theme key not found: $wanted_key" >&2
    return 1
end

function hex_to_rgb --argument-names hex_color
    set h (string replace '#' '' $hex_color)
    set r (math "0x"(string sub --start 1 --length 2 $h))
    set g (math "0x"(string sub --start 3 --length 2 $h))
    set b (math "0x"(string sub --start 5 --length 2 $h))
    echo "$r, $g, $b"
end

# ---------------------------------------------------------------------------
# Write a lua env-patch file so running nvim instances can update their
# stale vim.env before rebuilding the theme.
# Written before the socket loop so it's ready when reload_nvchad_theme runs.
# ---------------------------------------------------------------------------
set nvim_patch_file /tmp/nvim_theme_patch.lua

printf 'vim.env.THEME_NAME    = "%s"\n' $THEME_NAME    > $nvim_patch_file
printf 'vim.env.THEME_VARIANT = "%s"\n' $THEME_VARIANT >> $nvim_patch_file
for i in (seq (count $theme_keys))
    printf 'vim.env["THEME_%s"] = "%s"\n' $theme_keys[$i] $theme_vals[$i] >> $nvim_patch_file
end

# ---------------------------------------------------------------------------
# Ghostty
# ---------------------------------------------------------------------------
set g_base00 (get_theme_val BASE00)
set g_base01 (get_theme_val BASE01)
set g_base02 (get_theme_val BASE02)
set g_base05 (get_theme_val BASE05)
set g_base07 (get_theme_val BASE07)
set g_base08 (get_theme_val BASE08)
set g_base09 (get_theme_val BASE09)
set g_base0a (get_theme_val BASE0A)
set g_base0b (get_theme_val BASE0B)
set g_base0c (get_theme_val BASE0C)
set g_base0d (get_theme_val BASE0D)
set g_base0e (get_theme_val BASE0E)
set g_base0f (get_theme_val BASE0F)

printf '%s\n' \
    "palette = 0=$g_base00"  \
    "palette = 1=$g_base08"  \
    "palette = 2=$g_base0b"  \
    "palette = 3=$g_base0a"  \
    "palette = 4=$g_base0d"  \
    "palette = 5=$g_base0e"  \
    "palette = 6=$g_base0c"  \
    "palette = 7=$g_base05"  \
    "palette = 8=$g_base01"  \
    "palette = 9=$g_base08"  \
    "palette = 10=$g_base0b" \
    "palette = 11=$g_base0a" \
    "palette = 12=$g_base0d" \
    "palette = 13=$g_base0e" \
    "palette = 14=$g_base0c" \
    "palette = 15=$g_base07" \
    "background           = $g_base00" \
    "foreground           = $g_base05" \
    "cursor-color         = $g_base05" \
    "selection-background = $g_base02" \
    "selection-foreground = $g_base05" \
    > ~/dots/ghostty/themes/active

# ---------------------------------------------------------------------------
# Hyprland
# ---------------------------------------------------------------------------
set h_border     (string replace '#' '' (get_theme_val BASE02))
set h_background (string replace '#' '' (get_theme_val DARKER_BLACK))

printf '%s\n' \
    "general {" \
    "    col.active_border   = rgb($h_border)" \
    "    col.inactive_border = rgb($h_border)" \
    "}" \
    "misc {" \
    "    background_color = rgb($h_background)" \
    "}" \
    > ~/dots/hypr/colors.conf

# ---------------------------------------------------------------------------
# Vicinae
# ---------------------------------------------------------------------------
set v_name         $THEME_NAME
set v_accent       (get_theme_val RED)
set v_bg           (get_theme_val BLACK)
set v_fg           (get_theme_val WHITE)
set v_darker_black (get_theme_val DARKER_BLACK)
set v_base01       (get_theme_val BASE01)
set v_base03       (get_theme_val BASE03)
set v_grey         (get_theme_val GREY)
set v_green        (get_theme_val GREEN)
set v_red          (get_theme_val RED)
set v_yellow       (get_theme_val YELLOW)
set v_teal         (get_theme_val TEAL)
set v_blue         (get_theme_val BLUE)
set v_pink         (get_theme_val PINK)
set v_baby_pink    (get_theme_val BABY_PINK)
set v_purple       (get_theme_val PURPLE)

printf '%s\n' \
    '[meta]' \
    "name        = \"$v_name\"" \
    'description = "Generated by set_theme.fish"' \
    'variant     = "dark"' \
    'inherits    = "vicinae-dark"' \
    '' \
    '[colors.core]' \
    "accent               = \"$v_accent\"" \
    "accent_foreground    = \"$v_darker_black\"" \
    "background           = \"$v_bg\"" \
    "foreground           = \"$v_fg\"" \
    "secondary_background = \"$v_bg\"" \
    'border               = "colors.core.background"' \
    '' \
    '[colors.main_window]' \
    'border = "colors.core.background"' \
    "footer = { background = \"colors.core.secondary_background\" }" \
    '' \
    '[colors.settings_window]' \
    'border = "colors.core.border"' \
    '' \
    '[colors.accents]' \
    "blue    = \"$v_teal\"" \
    "green   = \"$v_green\"" \
    "magenta = \"$v_baby_pink\"" \
    "orange  = \"$v_pink\"" \
    "red     = \"$v_red\"" \
    "yellow  = \"$v_yellow\"" \
    "cyan    = \"$v_teal\"" \
    "purple  = \"$v_purple\"" \
    '' \
    '[colors.shortcut]' \
    'border = "colors.core.border"' \
    '' \
    '[colors.text]' \
    "default     = \"$v_fg\"" \
    "muted       = \"$v_base03\"" \
    "danger      = \"$v_red\"" \
    "success     = \"$v_green\"" \
    "placeholder = \"$v_base03\"" \
    "selection   = { background = \"$v_grey\", foreground = \"$v_fg\" }" \
    '' \
    '[colors.text.links]' \
    "default = \"$v_teal\"" \
    "visited = \"$v_blue\"" \
    '' \
    '[colors.input]' \
    "border       = \"$v_bg\"" \
    "border_focus = \"$v_teal\"" \
    "border_error = \"$v_red\"" \
    '' \
    '[colors.button.primary]' \
    "background = \"$v_darker_black\"" \
    "foreground = \"$v_fg\"" \
    "hover      = { background = \"$v_darker_black\" }" \
    'focus      = { outline = "colors.core.accent" }' \
    '' \
    '[colors.list.item.hover]' \
    "foreground           = \"$v_fg\"" \
    "secondary_foreground = \"$v_base03\"" \
    '' \
    '[colors.list.item.selection]' \
    "background           = \"$v_base01\"" \
    "foreground           = \"$v_fg\"" \
    "secondary_background = \"$v_bg\"" \
    "secondary_foreground = \"$v_base03\"" \
    '' \
    '[colors.grid.item]' \
    "background = \"$v_bg\"" \
    "hover      = { outline = \"$v_fg\" }" \
    "selection  = { outline = \"$v_fg\" }" \
    '' \
    '[colors.scrollbars]' \
    "background = \"$v_bg\"" \
    '' \
    '[colors.loading]' \
    "bar     = \"$v_teal\"" \
    "spinner = \"$v_teal\"" \
    > ~/dots/vicinae-themes/$v_name.toml

and echo "set_theme.fish: wrote ~/dots/vicinae-themes/$v_name.toml"
or  echo "set_theme.fish: failed to write vicinae config" >&2
vicinae theme set $v_name

# ---------------------------------------------------------------------------
# swaync
# ---------------------------------------------------------------------------
set sw_base00       (get_theme_val BASE00)
set sw_one_bg       (get_theme_val ONE_BG)
set sw_one_bg2      (get_theme_val ONE_BG2)
set sw_one_bg3      (get_theme_val ONE_BG3)
set sw_black2       (get_theme_val BLACK2)
set sw_darker_black (get_theme_val DARKER_BLACK)
set sw_cyan         (get_theme_val CYAN)
set sw_white        (get_theme_val WHITE)
set sw_base03       (get_theme_val BASE03)
set sw_blue         (get_theme_val BLUE)

set sw_base00_rgb       (hex_to_rgb $sw_base00)
set sw_one_bg_rgb       (hex_to_rgb $sw_one_bg)
set sw_one_bg2_rgb      (hex_to_rgb $sw_one_bg2)
set sw_one_bg3_rgb      (hex_to_rgb $sw_one_bg3)
set sw_black2_rgb       (hex_to_rgb $sw_black2)
set sw_darker_black_rgb (hex_to_rgb $sw_darker_black)
set sw_cyan_rgb         (hex_to_rgb $sw_cyan)
set sw_white_rgb        (hex_to_rgb $sw_white)
set sw_base03_rgb       (hex_to_rgb $sw_base03)
set sw_blue_rgb         (hex_to_rgb $sw_blue)

printf '%s\n' \
    ':root {' \
    "  --cc-bg:                      rgba($sw_base00_rgb, 1);" \
    '' \
    "  --noti-border-color:          rgba($sw_cyan_rgb, 0.15);" \
    '' \
    "  --noti-bg:                    $sw_one_bg_rgb;" \
    "  --noti-bg-alpha:              1;" \
    "  --noti-bg-darker:             rgb($sw_darker_black_rgb);" \
    "  --noti-bg-hover:              rgb($sw_one_bg2_rgb);" \
    "  --noti-bg-focus:              rgba($sw_cyan_rgb, 0.2);" \
    '' \
    "  --noti-close-bg:              rgb($sw_black2_rgb);" \
    "  --noti-close-bg-hover:        rgb($sw_one_bg3_rgb);" \
    '' \
    "  --text-color:                 rgb($sw_white_rgb);" \
    "  --text-color-disabled:        rgb($sw_base03_rgb);" \
    '' \
    "  --bg-selected:                rgb($sw_blue_rgb);" \
    '' \
    '  --notification-icon-size:       64px;' \
    '  --notification-app-icon-size:   calc(var(--notification-icon-size) / 3);' \
    '  --notification-group-icon-size: 32px;' \
    '' \
    '  --border:                     0px solid var(--noti-border-color);' \
    '  --border-radius:              12px;' \
    '' \
    '  --notification-shadow:' \
    '    0 0 0 1px rgba(0, 0, 0, 0.45),' \
    '    0 2px 6px rgba(0, 0, 0, 0.65);' \
    '' \
    '  --font-size-body:             15px;' \
    '  --font-size-summary:          16px;' \
    '' \
    '  --hover-transition:           background 0.15s ease-in-out;' \
    '  --group-collapse-transition:  opacity 400ms ease-in-out;' \
    '}' \
    '' \
    '/* GTK fallback */' \
    "@define-color cc-bg                  rgba($sw_base00_rgb, 1);" \
    "@define-color noti-border-color      rgba($sw_cyan_rgb, 0.15);" \
    '' \
    "@define-color noti-bg                rgba($sw_one_bg_rgb, 1);" \
    "@define-color noti-bg-opaque         rgb($sw_one_bg_rgb);" \
    "@define-color noti-bg-darker         rgb($sw_darker_black_rgb);" \
    '' \
    "@define-color noti-bg-hover          rgb($sw_one_bg2_rgb);" \
    "@define-color noti-bg-hover-opaque   rgb($sw_one_bg2_rgb);" \
    '' \
    "@define-color noti-bg-focus          rgba($sw_cyan_rgb, 0.2);" \
    '' \
    "@define-color noti-close-bg          rgba($sw_black2_rgb, 1);" \
    "@define-color noti-close-bg-hover    rgba($sw_one_bg3_rgb, 1);" \
    '' \
    "@define-color text-color             rgb($sw_white_rgb);" \
    "@define-color text-color-disabled    rgb($sw_base03_rgb);" \
    '' \
    "@define-color bg-selected            rgb($sw_blue_rgb);" \
    '' \
    > /tmp/swaync_vars.css

if not test -f ~/dots/swaync/styles_rules.css
    echo "set_theme.fish: ~/dots/swaync/styles_rules.css not found — swaync styles.css not written" >&2
else
    cat /tmp/swaync_vars.css ~/dots/swaync/styles_rules.css > ~/dots/swaync/style.css
    and echo "set_theme.fish: wrote ~/dots/swaync/styles.css"
    or  echo "set_theme.fish: failed to write swaync config" >&2

    if command -q swaync-client; and pgrep --exact swaync > /dev/null
        swaync-client -rs
        and echo "set_theme.fish: reloaded swaync"
        or  echo "set_theme.fish: swaync reload failed" >&2
    end
end

# ---------------------------------------------------------------------------
# Reload running nvim instances via their sockets.
# The patch file written above is dofile'd inside reload_nvchad_theme()
# to update stale vim.env before the theme is rebuilt.
# ---------------------------------------------------------------------------
set runtime_dir $XDG_RUNTIME_DIR
if test -z "$runtime_dir"
    set runtime_dir /run/user/(id -u)
end

set nvim_sockets
for s in $runtime_dir/nvim.*/0
    if test -S "$s"; and test "$s" != "$NVIM_LISTEN_ADDRESS"
        set --append nvim_sockets $s
    end
end

for socket in $nvim_sockets
    set result (nvim --server $socket --remote-expr 'v:lua.reload_nvchad_theme()' 2>/dev/null)
    if test $status -eq 0
        echo "set_theme.fish: reloaded nvim at $socket"
    else
        echo "set_theme.fish: could not reload nvim at $socket" >&2
    end
end

# Recompile the base46 highlight cache so the next nvim launch is correct.
nvim --headless -c 'lua require("base46").compile()' -c 'qa' 2>/dev/null
and echo "set_theme.fish: recompiled base46 cache"
or  echo "set_theme.fish: base46 cache recompile failed" >&2

# ---------------------------------------------------------------------------
# Persist the hash now that everything succeeded.
# ---------------------------------------------------------------------------
set --universal THEME_HASH $new_hash

echo "set_theme.fish: loaded '$THEME_NAME'"
