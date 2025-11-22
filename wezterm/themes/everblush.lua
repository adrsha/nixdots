-- Everblush color theme for WezTerm
local M = {}

M.colors = {
    -- Primary colors
    foreground = '#dadada',
    -- background = '#0e1315',
    background = '#000000',

    -- Cursor colors
    cursor_bg = '#dadada',
    cursor_fg = '#141b1e',
    cursor_border = '#dadada',

    -- Selection colors
    selection_fg = '#141b1e',
    selection_bg = '#dadada',

    -- Scrollbar thumb color
    scrollbar_thumb = '#3c474a',

    -- Split line color
    split = '#3c474a',

    -- ANSI colors (0-7) - normal colors
    ansi = {
        '#232a2d', -- black
        '#e57474', -- red
        '#8ccf7e', -- green
        '#e5c76b', -- yellow
        '#67b0e8', -- blue
        '#c47fd5', -- magenta
        '#6cbfbf', -- cyan
        '#b3b9b8', -- white
    },

    -- Bright ANSI colors (8-15)
    brights = {
        '#2d3437', -- bright black
        '#ef7e7e', -- bright red
        '#a3d98c', -- bright green
        '#f4d67a', -- bright yellow
        '#79bce8', -- bright blue
        '#ce8ee3', -- bright magenta
        '#7cd7d7', -- bright cyan
        '#d3dbe0', -- bright white
    },

    -- Colors for copy_mode and quick_select
    copy_mode_active_highlight_bg = { Color = '#e5c76b' },
    copy_mode_active_highlight_fg = { Color = '#141b1e' },
    copy_mode_inactive_highlight_bg = { Color = '#3c474a' },
    copy_mode_inactive_highlight_fg = { Color = '#dadada' },

    quick_select_label_bg = { Color = '#67b0e8' },
    quick_select_label_fg = { Color = '#141b1e' },
    quick_select_match_bg = { Color = '#c47fd5' },
    quick_select_match_fg = { Color = '#141b1e' },
}

return M
