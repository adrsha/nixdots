#!/usr/bin/env fish
# _ghostty.fish — write the Ghostty colour palette.

printf '%s\n' \
    "palette = 0="(get_theme_val BASE00)  \
    "palette = 1="(get_theme_val BASE08)  \
    "palette = 2="(get_theme_val BASE0B)  \
    "palette = 3="(get_theme_val BASE0A)  \
    "palette = 4="(get_theme_val BASE0D)  \
    "palette = 5="(get_theme_val BASE0E)  \
    "palette = 6="(get_theme_val BASE0C)  \
    "palette = 7="(get_theme_val BASE05)  \
    "palette = 8="(get_theme_val BASE01)  \
    "palette = 9="(get_theme_val BASE08)  \
    "palette = 10="(get_theme_val BASE0B) \
    "palette = 11="(get_theme_val BASE0A) \
    "palette = 12="(get_theme_val BASE0D) \
    "palette = 13="(get_theme_val BASE0E) \
    "palette = 14="(get_theme_val BASE0C) \
    "palette = 15="(get_theme_val BASE07) \
    "background           = "(get_theme_val BASE00) \
    "foreground           = "(get_theme_val BASE05) \
    "cursor-color         = "(get_theme_val BASE05) \
    "selection-background = "(get_theme_val BASE02) \
    "selection-foreground = "(get_theme_val BASE05) \
    > ~/dots/ghostty/themes/active
