-- Aquarium color theme for WezTerm
local M = {}

M.colors = {
  -- Primary colors
  foreground = '#63718B',
  background = '#20202A',
  -- Cursor colors
  cursor_bg = '#b8dceb',
  cursor_fg = '#20202a',
  cursor_border = '#b8dceb',

  -- Selection colors (using the original values from kitty config)
  selection_fg = '#2C2E3E',
  selection_bg = '#E5E9F0',

  -- The color of the scrollbar "thumb"
  scrollbar_thumb = '#1A1A22',

  -- The color of the split lines between panes
  split = '#3b3b4d',

  -- ANSI colors (0-7) - normal colors
  ansi = {
    '#1A1A22', -- black
    '#ebb9b9', -- red
    '#caf6bb', -- green
    '#ebe3b9', -- yellow
    '#b8cbfe', -- blue
    '#f6bbe7', -- magenta
    '#b8dceb', -- cyan
    '#C6D0E9', -- white
  },
  -- Bright ANSI colors (8-15)
  brights = {
    '#3b3b4d', -- bright black
    '#cc9b9d', -- bright red
    '#a3ccad', -- bright green
    '#d1ba97', -- bright yellow
    '#B8C9EA', -- bright blue
    '#c497b3', -- bright magenta
    '#95C2D1', -- bright cyan
    '#63718B', -- bright white
  },

  -- Colors for copy_mode and quick_select
  copy_mode_active_highlight_bg = { Color = '#E5E9F0' },
  copy_mode_active_highlight_fg = { Color = '#2C2E3E' },
  copy_mode_inactive_highlight_bg = { Color = '#1A1A22' },
  copy_mode_inactive_highlight_fg = { Color = '#C6D0E9' },

  quick_select_label_bg = { Color = '#cddbf9' },
  quick_select_label_fg = { Color = '#20202A' },
  quick_select_match_bg = { Color = '#f6bbe7' },
  quick_select_match_fg = { Color = '#20202A' },
}

return M
