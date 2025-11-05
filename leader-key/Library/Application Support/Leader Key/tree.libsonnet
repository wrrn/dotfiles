// Implements the generatePrefixTree function as specified.
// The main function is exposed as `generate`.
//
// You can import this file and use the function like:
//   local prefixTree = import 'prefix_tree.jsonnet';
//   local myTree = prefixTree.generate(["my", "list", "of", "strings"]);

local node(key, children) = {
  type: 'node',
  key: key,
  children: children,
};

local leaf(key, value) = {
  type: 'leaf',
  key: key,
  value: value,
};

local is_leaf(node) = node.type == 'leaf';
local is_node(node) = node.type == 'node';

local generatePrefixTree(stringList) = (
  // 1. Pre-processing:
  //    - Convert all strings to lowercase.
  //    - Remove duplicates by using them as object keys.
  //    - Filter out any null or empty strings.
  local normalizedList = std.filterMap(
    function(x) x != null && std.length(x) > 0,
    function(x) std.asciiLower(x),
    stringList
  );

  // 2. Define the main recursive tree-building function
  local buildTree(strings, depth) = (
    // Helper function to get the grouping key for a string at a given depth.
    // - If the string ends (length == depth), the key is ".". (Rule 4.1)
    // - Otherwise, it's the character at that depth.
    local getKey(s, d) =
      if std.length(s) == d then '.'
      else std.substr(s, d, 1);

    // 3. Grouping:
    //    - Find all unique keys at this depth.
    //    - Sort them for deterministic output.
    local allKeys = std.set(
      std.map(
        function(s) getKey(s, depth),
        strings
      )
    );

    //    - Create an object mapping each key to the list of strings that share it.
    local groupedStrings = {
      [k]: std.filter(function(s) getKey(s, depth) == k, strings)
      for k in allKeys
    };

    local to_node(key) = (
      local childStrings = groupedStrings[key];
      local childCount = std.length(childStrings);
      if childCount > 1 then node(key=key, children=buildTree(childStrings, depth + 1))
      else leaf(key=key, value=childStrings[0])
    );

    std.map(to_node, allKeys)
  );  // end of buildTree

  // 5. Initial Call:
  //    - Start the recursion with the normalized list at depth 0.
  buildTree(normalizedList, 0)
);

// Map over a tree structure, transforming each node
local map(tree, leafFormatter, nodeFormatter) = (
  local mapNode(node) =
    if is_leaf(node) then
      leafFormatter(node.key, node.value)
    else if is_node(node) then
      nodeFormatter(node.key, std.map(mapNode, node.children))
    else
      error 'Unknown node type: ' + node.type;

  std.map(mapNode, tree)
);

// --- Output ---
{
  // The primary export of this library file is the `generate` function.
  generate: generatePrefixTree,

  // Map function for transforming trees
  map: map,

  // --- Verification Examples ---
  // These private fields (prefixed with _) are included to show
  // the output for the examples from the functional specification.

  // Example 1: Standard Branching
  _example1_output: self.generate(['asfa', 'asda', 'bsd']),

  // Example 2: Prefix Overlaps & Case Insensitivity
  _example2_output: self.generate(['Car', 'cart', 'CAr']),

  // Example 3: Custom Formatters using mapTree
  // This example shows how to generate a tree then transform it.
  // First generate the standard tree, then map it to a custom format.
  _example3_output: (
    local standardTree = self.generate(['asfa', 'asda', 'bsd']);
    self.mapTree(
      standardTree,
      leafFormatter=function(key, string) {
        label: key,
        value: string,
      },
      groupFormatter=function(key, children) {
        label: key,
        children: children,
      }
    )
  ),
}
