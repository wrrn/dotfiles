local colors = require("colors")

-- Equivalent to the --bar domain
sbar.bar({
    height = 20,
    notch_display_height = 20,
    topmost = true,
    color = colors.bar.bg,
    blur_radius = 30,
    padding_right = 2,
    padding_left = 2,
})
