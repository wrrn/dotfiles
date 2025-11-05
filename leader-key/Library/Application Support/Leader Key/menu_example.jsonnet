local tree = import 'tree.libsonnet';

// Example: Generate a keyboard shortcut menu structure
local shortcuts = ["save", "search", "sort", "select", "undo", "redo"];

tree.generate(
  shortcuts,
  // Leaf formatter: creates an actionable menu item
  leafFormatter=function(key, string) {
    key: key,
    command: string,
    description: "Execute " + string,
  },
  // Group formatter: creates a submenu
  groupFormatter=function(key, actions) {
    key: key,
    type: "submenu",
    items: actions,
  }
)
