# module/_wallpaper.fish — set wallpaper via awww

if test -z "$THEME_WALLPAPER"
    return 0
end

if not test -f "$THEME_WALLPAPER"
    killall awww -q
    return 0
end

if command -q awww; and awww query >/dev/null 2>&1
    awww img "$THEME_WALLPAPER"
    if test $status -eq 0
        echo "set_theme.fish: set wallpaper $THEME_WALLPAPER"
    else
        echo "set_theme.fish: awww failed" >&2
    end
else
    nohup setsid awww-daemon >/dev/null 2>&1 &
    disown
    echo "set_theme.fish: awww not running — skipping wallpaper" >&2
end
