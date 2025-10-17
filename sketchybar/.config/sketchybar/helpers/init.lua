-- Add the sketchybar module to the package cpath
package.cpath = package.cpath .. ";/Users/" .. os.getenv("USER") .. "/.local/share/sketchybar_lua/?.so"

print("Here we go!!!")
os.execute("(cd helpers && make)")
print("Leaving helpers")
