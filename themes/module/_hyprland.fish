#!/usr/bin/env fish
# _hyprland.fish — write Hyprland colour config.

set -l border     (string replace '#' '' (get_theme_val BASE02))
set -l background (string replace '#' '' (get_theme_val DARKER_BLACK))

printf '%s\n' \
    "general {" \
    "    col.active_border   = rgb($border)" \
    "    col.inactive_border = rgb($border)" \
    "}" \
    "misc {" \
    "    background_color = rgb($background)" \
    "}" \
    > ~/dots/hypr/colors.conf
