#!/usr/bin/env bash

status=$(playerctl status 2>/dev/null)
artist=$(playerctl metadata artist 2>/dev/null)
title=$(playerctl metadata title 2>/dev/null)

if [ "$status" = "Playing" ]; then
    icon=" "
elif [ "$status" = "Paused" ]; then
    icon=" "
else
    echo ""
    exit 0
fi

# Truncate if too long
artist=$(echo "$artist" | cut -c1-15)
title=$(echo "$title" | cut -c1-20)

echo "$icon $title - $artist"
