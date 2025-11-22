-- Catppuccin Mocha theme for WezTerm
local M = {};

M.colors = {
  -- Primary colors
  foreground        = '#cdd6f4', -- Text
  -- background        = '#181724', -- Base
  background        = '#000000', -- Base

  -- Cursor colors
  cursor_bg         = '#f5e0dc', -- Rosewater
  cursor_fg         = '#181724', -- Base
  cursor_border     = '#f5e0dc', -- Rosewater

  -- Selection colors
  selection_fg      = '#1e1e2e', -- Base
  selection_bg      = '#f5e0dc', -- Rosewater

  -- The color of the scrollbar "thumb"
  scrollbar_thumb   = '#13121D', -- Mantle

  -- The color of the split lines between panes
  split             = '#1e1e2e', -- Surface 0

  -- ANSI colors (0-7) - normal colors
  ansi = {
    '#313244', -- black (Surface 1)
    '#f38ba8', -- red (Red)
    '#a6e3a1', -- green (Green)
    '#f9e2af', -- yellow (Yellow)
    '#89b4fa', -- blue (Blue)
    '#f5c2e7', -- magenta (Pink)
    '#94e2d5', -- cyan (Teal)
    '#bac2de', -- white (Subtext 1)
  },

  -- Bright ANSI colors (8-15)
  brights = {
    '#585b70', -- bright black (Surface 2)
    '#f38ba8', -- bright red (Red)
    '#a6e3a1', -- bright green (Green)
    '#f9e2af', -- bright yellow (Yellow)
    '#89b4fa', -- bright blue (Blue)
    '#f5c2e7', -- bright magenta (Pink)
    '#94e2d5', -- bright cyan (Teal)
    '#a6adc8', -- bright white (Subtext 0)
  },

  -- Colors for copy_mode and quick_select
  copy_mode_active_highlight_bg       = { Color = '#f5e0dc' }, -- Rosewater
  copy_mode_active_highlight_fg       = { Color = '#181724' }, -- Base
  copy_mode_inactive_highlight_bg     = { Color = '#1e1e2e' }, -- Surface 0
  copy_mode_inactive_highlight_fg     = { Color = '#cdd6f4' }, -- Text
  quick_select_label_bg               = { Color = '#89b4fa' }, -- Blue
  quick_select_label_fg               = { Color = '#181724' }, -- Base
  quick_select_match_bg               = { Color = '#f5c2e7' }, -- Pink
  quick_select_match_fg               = { Color = '#181724' }, -- Base
};

return M;
