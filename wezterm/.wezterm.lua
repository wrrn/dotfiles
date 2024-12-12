-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices
-- For example, changing the color scheme:
config.color_scheme = 'Kanagawa (Gogh)'

config.window_decorations = "RESIZE"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.hide_tab_bar_if_only_one_tab = true
config.enable_tab_bar = true
config.font = wezterm.font 'Ellograph CF'
config.font_size = 16.0

config.leader = { key = 'x', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
   {
      key = '%',
      mods = 'LEADER',
      action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
   },
   {
      key = '"',
      mods = 'LEADER',
      action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
   },
   {
      key = 'p',
      mods = 'LEADER',
      action = wezterm.action.ActivateKeyTable {
         name = 'pane',
         one_shot = true,
      },
   },
   {
      key = 's',
      mods = 'CTRL',
      action = wezterm.action.Search {CaseInSensitiveString = ''},
   },
   {
      key = 'Space',
      mods = 'CTRL',
      action = wezterm.action.ActivateCopyMode ,
   },
   {
      key = 'q',
      mods = 'LEADER',
      action = wezterm.action.PaneSelect,
   },
   {
      key = 'o',
      mods = 'LEADER',
      action = wezterm.action.ActivatePaneDirection 'Next',
   },
   {
      key = '1',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(0),
   },
   {
      key = '2',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(1),
   },
   {
      key = '3',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(2),
   },
   {
      key = '4',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(3),
   },
   {
      key = '6',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(5),
   },
   {
      key = '7',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(6),
   },
   {
      key = '8',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(7),
   },
   {
      key = '9',
      mods = 'LEADER',
      action = wezterm.action.ActivateTab(8),
   },
}

config.key_tables = {
   pane = {
      { key = "z", action = wezterm.action.TogglePaneZoomState },
      { key = "k", action = wezterm.action.CloseCurrentPane { confirm = false }},
      { key = "s", action = wezterm.action.ActivateKeyTable {
           name = "pane_split",
           one_shot = true,
      }}
   },
   pane_split = {
      { key = "h", action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' } },
      { key = "v", action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' } }
   }
}


config.enable_wayland = true
config.front_end="WebGpu"

-- and finally, return the configuration to wezterm
return config



