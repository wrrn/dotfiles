local settings = require("settings")
local colors = require("colors")

-- Padding item required because of bracket
-- sbar.add("item", { position = "right", width = settings.group_paddings })

local cal = sbar.add("item", "calendar", {
  label = {
    color = colors.white,
    padding_right = 8,
    width = 49,
    align = "right",
    font = { family = settings.font.numbers },
  },
  position = "right",
  update_freq = 30,
  padding_left = 1,
  padding_right = 15,
  -- Adding Keyboard shortcut to open Grila
  click_script = "osascript -e "..
    "'tell application \"System Events\" to keystroke \";\" using {command down}'" 
})

cal:subscribe({ "forced", "routine", "system_woke" }, function(env)
  cal:set({ label = os.date("%H:%M") })
end)
