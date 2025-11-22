-- Base30 color theme for WezTerm
local M = {}

M.colors = {
    -- Primary colors
    foreground = '#c7ccd1',
    background = '#1c2023',

    -- Cursor colors
    cursor_bg = '#c7ccd1',
    cursor_fg = '#1c2023',
    cursor_border = '#c7ccd1',

    -- Selection colors
    selection_fg = '#1c2023',
    selection_bg = '#c7ccd1',

    -- The color of the scrollbar "thumb"
    scrollbar_thumb = '#44484b',

    -- The color of the split lines between panes
    split = '#303437',

    -- ANSI colors (0-7) - normal colors
    ansi = {
        '#161a1d', -- black (darker_black)
        '#c79595', -- red
        '#aec795', -- green
        '#c7c795', -- yellow
        '#95aec7', -- blue
        '#ae95c7', -- magenta (purple)
        '#95c7ae', -- cyan (vibrant_green)
        '#c7ccd1', -- white
    },

    -- Bright ANSI colors (8-15)
    brights = {
        '#44484b', -- bright black (grey)
        '#c79595', -- bright red
        '#aec795', -- bright green
        '#d0d09e', -- bright yellow (sun)
        '#8ca5be', -- bright blue (nord_blue)
        '#a58cbe', -- bright magenta (dark_purple)
        '#9eb7d0', -- bright cyan
        '#f3f4f5', -- bright white (base07)
    },

    -- Colors for copy_mode and quick_select
    copy_mode_active_highlight_bg = { Color = '#c7ccd1' },
    copy_mode_active_highlight_fg = { Color = '#1c2023' },
    copy_mode_inactive_highlight_bg = { Color = '#44484b' },
    copy_mode_inactive_highlight_fg = { Color = '#c7ccd1' },

    quick_select_label_bg = { Color = '#95aec7' },
    quick_select_label_fg = { Color = '#1c2023' },
    quick_select_match_bg = { Color = '#ae95c7' },
    quick_select_match_fg = { Color = '#1c2023' },
}

return M
