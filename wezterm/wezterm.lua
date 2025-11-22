local wezterm = require 'wezterm'
local theme = require 'themes.everblush'

local config = wezterm.config_builder()

-- Color & UI Settings
config.colors = theme.colors
config.enable_tab_bar = false
config.enable_wayland = true
config.enable_scroll_bar = false
config.window_background_opacity = 1
config.window_padding = {
    left = 40,
    right = 40,
    top = 40,
    bottom = 40,
}
config.window_close_confirmation = "NeverPrompt"

-- Font
config.font = wezterm.font_with_fallback {
    -- {
    --     family = 'Dank Mono',
    --     weight = 'Bold',
    --     harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },
    -- },
    {
        family = 'JetBrains Mono Nerd Font',
        -- harfbuzz_features = { 'calt=1', 'clig=1', 'liga=1' },
        weight = 'DemiBold',
    },
    -- {
    --     family = 'Adwaita Mono Nerd Font',
    --     harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },
    --     weight = 'Regular',
    -- }
}
config.font_size = 13
config.line_height = 1.4

-- Disable defaults, then rebuild only what you need
config.disable_default_key_bindings = true

-- Key Bindings (ALT replaces SUPER)
config.keys = {
    -- Copy / Paste
    { key = 'C',          mods = 'ALT',            action = wezterm.action.CopyTo 'Clipboard' },
    { key = 'V',          mods = 'ALT',            action = wezterm.action.PasteFrom 'Clipboard' },
    { key = 'C',          mods = 'CTRL|SHIFT',     action = wezterm.action.CopyTo 'Clipboard' },
    { key = 'V',          mods = 'CTRL|SHIFT',     action = wezterm.action.PasteFrom 'Clipboard' },
    { key = 'Insert',     mods = 'CTRL',           action = wezterm.action.CopyTo 'PrimarySelection' },
    { key = 'Insert',     mods = 'SHIFT',          action = wezterm.action.PasteFrom 'PrimarySelection' },
    { key = 'Copy',       mods = '',               action = wezterm.action.CopyTo 'Clipboard' },
    { key = 'Paste',      mods = '',               action = wezterm.action.PasteFrom 'Clipboard' },

    -- Window management
    { key = 'm',          mods = 'ALT',            action = wezterm.action.Hide },
    { key = 'n',          mods = 'ALT',            action = wezterm.action.SpawnWindow },
    { key = 'n',          mods = 'CTRL|SHIFT',     action = wezterm.action.SpawnWindow },
    { key = 'Enter',      mods = 'ALT',            action = wezterm.action.ToggleFullScreen },
    { key = '-',          mods = 'ALT',            action = wezterm.action.DecreaseFontSize },
    { key = '=',          mods = 'ALT',            action = wezterm.action.IncreaseFontSize },
    { key = '0',          mods = 'ALT',            action = wezterm.action.ResetFontSize },
    { key = 'r',          mods = 'ALT',            action = wezterm.action.ReloadConfiguration },
    { key = 'R',          mods = 'CTRL|SHIFT',     action = wezterm.action.ReloadConfiguration },

    -- Tabs
    { key = 't',          mods = 'ALT',            action = wezterm.action.SpawnTab 'CurrentPaneDomain' },
    { key = 't',          mods = 'CTRL|SHIFT',     action = wezterm.action.SpawnTab 'CurrentPaneDomain' },
    { key = 'T',          mods = 'ALT|SHIFT',      action = wezterm.action.SpawnTab 'DefaultDomain' },
    { key = 'w',          mods = 'ALT',            action = wezterm.action.CloseCurrentTab { confirm = true } },
    { key = 'w',          mods = 'CTRL|SHIFT',     action = wezterm.action.CloseCurrentTab { confirm = true } },

    -- Tab activation (ALT 1–9 and CTRL+SHIFT 1–9)
    { key = '1',          mods = 'ALT',            action = wezterm.action.ActivateTab(0) },
    { key = '2',          mods = 'ALT',            action = wezterm.action.ActivateTab(1) },
    { key = '3',          mods = 'ALT',            action = wezterm.action.ActivateTab(2) },
    { key = '4',          mods = 'ALT',            action = wezterm.action.ActivateTab(3) },
    { key = '5',          mods = 'ALT',            action = wezterm.action.ActivateTab(4) },
    { key = '6',          mods = 'ALT',            action = wezterm.action.ActivateTab(5) },
    { key = '7',          mods = 'ALT',            action = wezterm.action.ActivateTab(6) },
    { key = '8',          mods = 'ALT',            action = wezterm.action.ActivateTab(7) },
    { key = '9',          mods = 'ALT',            action = wezterm.action.ActivateTab(-1) },


    { key = '[',          mods = 'ALT|SHIFT',      action = wezterm.action.ActivateTabRelative(-1) },
    { key = ']',          mods = 'ALT|SHIFT',      action = wezterm.action.ActivateTabRelative(1) },
    { key = 'Tab',        mods = 'CTRL|SHIFT',     action = wezterm.action.ActivateTabRelative(-1) },
    { key = 'PageUp',     mods = 'CTRL|SHIFT',     action = wezterm.action.MoveTabRelative(-1) },
    { key = 'PageDown',   mods = 'CTRL|SHIFT',     action = wezterm.action.MoveTabRelative(1) },

    -- Scrolling
    { key = 'PageUp',     mods = 'SHIFT',          action = wezterm.action.ScrollByPage(-1) },
    { key = 'PageDown',   mods = 'SHIFT',          action = wezterm.action.ScrollByPage(1) },

    -- Search & overlays
    { key = 'f',          mods = 'ALT',            action = wezterm.action.Search { CaseSensitiveString = "" } },
    { key = 'F',          mods = 'CTRL|SHIFT',     action = wezterm.action.Search { CaseSensitiveString = "" } },
    { key = 'P',          mods = 'CTRL|SHIFT',     action = wezterm.action.ActivateCommandPalette },
    { key = 'U',          mods = 'CTRL|SHIFT',     action = wezterm.action.CharSelect },
    { key = 'L',          mods = 'CTRL|SHIFT',     action = wezterm.action.ShowDebugOverlay },
    { key = 'K',          mods = 'ALT',            action = wezterm.action.ClearScrollback 'ScrollbackOnly' },
    { key = 'K',          mods = 'CTRL|SHIFT',     action = wezterm.action.ClearScrollback 'ScrollbackOnly' },

    -- Panes
    { key = '"',          mods = 'CTRL|SHIFT|ALT', action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' } },
    { key = '%',          mods = 'CTRL|SHIFT|ALT', action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' } },
    { key = 'LeftArrow',  mods = 'CTRL|SHIFT|ALT', action = wezterm.action.AdjustPaneSize { 'Left', 1 } },
    { key = 'RightArrow', mods = 'CTRL|SHIFT|ALT', action = wezterm.action.AdjustPaneSize { 'Right', 1 } },
    { key = 'UpArrow',    mods = 'CTRL|SHIFT|ALT', action = wezterm.action.AdjustPaneSize { 'Up', 1 } },
    { key = 'DownArrow',  mods = 'CTRL|SHIFT|ALT', action = wezterm.action.AdjustPaneSize { 'Down', 1 } },
    { key = 'h',  mods = 'CTRL|SHIFT',     action = wezterm.action.ActivatePaneDirection 'Left' },
    { key = 'l', mods = 'CTRL|SHIFT',     action = wezterm.action.ActivatePaneDirection 'Right' },
    { key = 'k',    mods = 'CTRL|SHIFT',     action = wezterm.action.ActivatePaneDirection 'Up' },
    { key = 'j',  mods = 'CTRL|SHIFT',     action = wezterm.action.ActivatePaneDirection 'Down' },
    { key = 'Z',          mods = 'CTRL|SHIFT',     action = wezterm.action.TogglePaneZoomState },

    -- Copy mode / QuickSelect
    { key = 'X',          mods = 'CTRL|SHIFT',     action = wezterm.action.ActivateCopyMode },
    { key = 'Space',      mods = 'CTRL|SHIFT',     action = wezterm.action.QuickSelect },
}

-- Plugin: Modal
local modal = wezterm.plugin.require("https://github.com/MLFlexer/modal.wezterm")
modal.enable_defaults("https://github.com/MLFlexer/modal.wezterm")

return config
