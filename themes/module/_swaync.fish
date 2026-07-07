#!/usr/bin/env fish
# _swaync.fish — write swaync CSS variables and reload the daemon.

set -l base00       (get_theme_val BASE00)
set -l one_bg       (get_theme_val ONE_BG)
set -l one_bg2      (get_theme_val ONE_BG2)
set -l one_bg3      (get_theme_val ONE_BG3)
set -l black2       (get_theme_val BLACK2)
set -l darker_black (get_theme_val DARKER_BLACK)
set -l cyan         (get_theme_val CYAN)
set -l white        (get_theme_val WHITE)
set -l base03       (get_theme_val BASE03)
set -l blue         (get_theme_val BLUE)

set -l r_base00       (hex_to_rgb $base00)
set -l r_one_bg       (hex_to_rgb $one_bg)
set -l r_one_bg2      (hex_to_rgb $one_bg2)
set -l r_one_bg3      (hex_to_rgb $one_bg3)
set -l r_black2       (hex_to_rgb $black2)
set -l r_darker_black (hex_to_rgb $darker_black)
set -l r_cyan         (hex_to_rgb $cyan)
set -l r_white        (hex_to_rgb $white)
set -l r_base03       (hex_to_rgb $base03)
set -l r_blue         (hex_to_rgb $blue)

printf '%s\n' \
    ':root {' \
    "  --cc-bg:                      rgba($r_base00, 1);" \
    '' \
    "  --noti-border-color:          rgba($r_cyan, 0.15);" \
    '' \
    "  --noti-bg:                    $r_one_bg;" \
    "  --noti-bg-alpha:              1;" \
    "  --noti-bg-darker:             rgb($r_darker_black);" \
    "  --noti-bg-hover:              rgb($r_one_bg2);" \
    "  --noti-bg-focus:              rgba($r_cyan, 0.2);" \
    '' \
    "  --noti-close-bg:              rgb($r_black2);" \
    "  --noti-close-bg-hover:        rgb($r_one_bg3);" \
    '' \
    "  --text-color:                 rgb($r_white);" \
    "  --text-color-disabled:        rgb($r_base03);" \
    '' \
    "  --bg-selected:                rgb($r_blue);" \
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
    "@define-color cc-bg                  rgba($r_base00, 1);" \
    "@define-color noti-border-color      rgba($r_cyan, 0.15);" \
    '' \
    "@define-color noti-bg                rgba($r_one_bg, 1);" \
    "@define-color noti-bg-opaque         rgb($r_one_bg);" \
    "@define-color noti-bg-darker         rgb($r_darker_black);" \
    '' \
    "@define-color noti-bg-hover          rgb($r_one_bg2);" \
    "@define-color noti-bg-hover-opaque   rgb($r_one_bg2);" \
    '' \
    "@define-color noti-bg-focus          rgba($r_cyan, 0.2);" \
    '' \
    "@define-color noti-close-bg          rgba($r_black2, 1);" \
    "@define-color noti-close-bg-hover    rgba($r_one_bg3, 1);" \
    '' \
    "@define-color text-color             rgb($r_white);" \
    "@define-color text-color-disabled    rgb($r_base03);" \
    '' \
    "@define-color bg-selected            rgb($r_blue);" \
    '' \
    > /tmp/swaync_vars.css

set -l rules ~/dots/swaync/styles_rules.css
if not test -f $rules
    echo "set_theme.fish: $rules not found — swaync style.css not written" >&2
    return 0
end

cat /tmp/swaync_vars.css $rules > ~/dots/swaync/style.css
and echo "set_theme.fish: wrote ~/dots/swaync/style.css"
or begin
    echo "set_theme.fish: failed to write swaync config" >&2
    return 0
end

if command -q swaync-client; and pgrep -x swaync > /dev/null
    swaync-client -rs
    and echo "set_theme.fish: reloaded swaync"
    or  echo "set_theme.fish: swaync reload failed" >&2
end
