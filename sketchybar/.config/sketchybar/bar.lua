local colors = require("colors")

-- Equivalent to the --bar domain
sbar.bar({
    height = 40,
    notch_display_height = 20,
    color = colors.bar.bg,
    blur_radius = 30,
    padding_right = 2,
    padding_left = 2,
})
