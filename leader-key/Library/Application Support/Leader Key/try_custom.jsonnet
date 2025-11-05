local tree = import 'tree.libsonnet';

// Example with custom formatters
local customTree = tree.generate(
  ["asfa", "asda", "bsd", "car", "cart"],
  leafFormatter=function(key, string) {
    label: key,
    value: string,
  },
  groupFormatter=function(key, actions) {
    label: key,
    children: actions,
  }
);

customTree
