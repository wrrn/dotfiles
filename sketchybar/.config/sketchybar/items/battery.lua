local colors = require("colors")
local settings = require("settings")

local battery = sbar.add("item", "widgets.battery", {
  position = "right",
  icon = {
      font = {
          family = "ZedMono Nerd Font",
          style = settings.font.style_map["Regular"],
      size = 19.0,
    }
  },
  label = { font = { family = settings.font.numbers } },
  update_freq = 180,
  popup = { align = "center" }
})

local remaining_time = sbar.add("item", {
  position = "popup." .. battery.name,
  icon = {
    string = "Time remaining:",
    width = 100,
    align = "left"
  },
  label = {
    string = "??:??h",
    width = 100,
    align = "right"
  },
})


battery:subscribe({"routine", "power_source_change", "system_woke"}, function()
  sbar.exec("pmset -g batt", function(batt_info)
    local icon = "!"
    local label = "No battery data"
    local color = colors.white
    
    local found, _, charge = batt_info:find("(%d+)%%")
    if found then
      charge = tonumber(charge)
      label = string.format("%02d%%", charge)
    end


    local charging, _, _ = batt_info:find("AC Power")
    print("here we are")

    if charging then
        icon = ""
        color = colors.white
    elseif found and charge > 10 then
        -- Hide everything if we are over 10
        icon = ""
        label = "" 
        color = colors.transparent
    elseif found and charge <= 10 then
        icon = ""
        color = colors.red
    end


    battery:set({
      icon = {
        string = icon,
        color = color
      },
      label = label,
    })
  end)
end)

battery:subscribe("mouse.clicked", function(env)
  local drawing = battery:query().popup.drawing
  battery:set( { popup = { drawing = "toggle" } })

  if drawing == "off" then
    sbar.exec("pmset -g batt", function(batt_info)
      local found, _, remaining = batt_info:find(" (%d+:%d+) remaining")
      local label = found and remaining .. "h" or "No estimate"
      remaining_time:set( { label = label })
    end)
  end
end)

-- sbar.add("bracket", "widgets.battery.bracket", { battery.name }, {
--   background = { color = colors.bg1 }
-- })

-- sbar.add("item", "widgets.battery.padding", {
--   position = "right",
--   width = settings.group_paddings
-- })
