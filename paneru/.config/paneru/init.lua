-- Paneru only loads init.lua, so this shim hands off to config.fnl.
-- Fennel comes from services.paneru.extraLuaPackages in nixos-config.
--
-- Paneru only watches init.lua for hot reload. After editing config.fnl,
-- re-save this file (or restart Paneru) to pick up the change.

local dir = (os.getenv("XDG_CONFIG_HOME") or (os.getenv("HOME") .. "/.config")) .. "/paneru"

local fennel = require("fennel")
fennel.path = dir .. "/?.fnl;" .. fennel.path
fennel.install()

fennel.dofile(dir .. "/config.fnl")
