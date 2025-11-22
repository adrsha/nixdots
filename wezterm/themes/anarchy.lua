-- Anarchy color theme for WezTerm
local M = {}

M.colors = {
    -- Primary colors
    foreground = '#444F4C', -- from URxvt.foreground
    background = '#131C19', -- from URxvt.background

    -- Cursor colors
    cursor_bg = '#252e2b', -- from URxvt.cursorColor
    cursor_fg = '#131C19', -- background
    cursor_border = '#252e2b',

    -- Selection colors
    selection_fg = '#131C19',
    selection_bg = '#444F4C',

    -- Scrollbar thumb color
    scrollbar_thumb = '#222e2b',

    -- Split line color
    split = '#222e2b',

    -- ANSI colors (0-7) - normal colors
    ansi = {
        '#101715', -- black (color0)
        '#4e3837', -- red (color1)
        '#2c3f33', -- green (color2)
        '#4e4737', -- yellow (color3)
        '#324448', -- blue (color4)
        '#373e4e', -- magenta (color5)
        '#374c4e', -- cyan (color6)
        '#222e2b', -- white (color7)
    },

    -- Bright ANSI colors (8-15)
    brights = {
        '#1C2623', -- bright black (color8)
        '#735959', -- bright red (color9)
        '#395242', -- bright green (color10)
        '#7a573f', -- bright yellow (color11)
        '#41575c', -- bright blue (color12)
        '#5e526a', -- bright magenta (color13)
        '#4c685f', -- bright cyan (color14)
        '#222e2b', -- bright white (color15)
    },

    -- Colors for copy_mode and quick_select
    copy_mode_active_highlight_bg = { Color = '#4e4737' },
    copy_mode_active_highlight_fg = { Color = '#131C19' },
    copy_mode_inactive_highlight_bg = { Color = '#222e2b' },
    copy_mode_inactive_highlight_fg = { Color = '#444F4C' },

    quick_select_label_bg = { Color = '#324448' },
    quick_select_label_fg = { Color = '#131C19' },
    quick_select_match_bg = { Color = '#373e4e' },
    quick_select_match_fg = { Color = '#131C19' },
}

return M
