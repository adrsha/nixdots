#!/usr/bin/env bash

# Path to the battery information
BATTERY_PATH="/sys/class/power_supply/BAT0"

# Check if battery exists
if [ ! -d "$BATTERY_PATH" ]; then
    echo "Battery not found"
    exit 1
fi

# Read battery status
capacity=$(cat "$BATTERY_PATH/capacity")
status=$(cat "$BATTERY_PATH/status")

# Determine charging status
is_charging="false"
if [ "$status" = "Charging" ] || [ "$status" = "Full" ]; then
    is_charging="true"
fi

# Output JSON for easy parsing
echo "{\"percentage\": $capacity, \"charging\": $is_charging}"
