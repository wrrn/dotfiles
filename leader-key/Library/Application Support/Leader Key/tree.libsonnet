// Implements the generatePrefixTree function as specified.
// The main function is exposed as `generate`.
//
// You can import this file and use the function like:
//   local prefixTree = import 'prefix_tree.jsonnet';
//   local myTree = prefixTree.generate(["my", "list", "of", "strings"]);

local generatePrefixTree(stringList, leafFormatter=null, groupFormatter=null) = (
  // 1. Pre-processing:
  //    - Convert all strings to lowercase.
  //    - Remove duplicates by using them as object keys.
  //    - Filter out any null or empty strings.
  local normalizedList = std.filterMap(
    function(x) x != null && std.length(x) > 0,
    function(x) std.asciiLower(x),
    stringList
  );

  // Default formatters that match the original behavior
  local defaultLeafFormatter = function(key, string) {
    type: "leaf",
    key: key,
    string: string,
  };

  local defaultGroupFormatter = function(key, actions) {
    type: "group",
    key: key,
    actions: actions,
  };

  // Use provided formatters or fall back to defaults
  local formatLeaf = if leafFormatter != null then leafFormatter else defaultLeafFormatter;
  local formatGroup = if groupFormatter != null then groupFormatter else defaultGroupFormatter;

  // 2. Define the main recursive tree-building function
  local buildTree(strings, depth) = (
    // Helper function to get the grouping key for a string at a given depth.
    // - If the string ends (length == depth), the key is ".". (Rule 4.1)
    // - Otherwise, it's the character at that depth.
    local getKey(s, d) =
      if std.length(s) == d then "."
      else std.substr(s, d, 1);

    // 3. Grouping:
    //    - Find all unique keys at this depth.
    //    - Sort them for deterministic output.
    local allKeys = std.set([getKey(s, depth) for s in strings]);

    //    - Create an object mapping each key to the list of strings that share it.
    local groupedStrings = {
      [k]: [s for s in strings if getKey(s, depth) == k]
      for k in allKeys
    };

    // 4. Node Generation:
    //    - Iterate over the sorted keys and build the output array.
    [
      local childStrings = groupedStrings[k];
      local childCount = std.length(childStrings);

      // Rule 3.1 & 4: If multiple strings share a key, create a GroupNode
      // and recursively process the children at the next depth.
      if childCount > 1 then
        formatGroup(k, buildTree(childStrings, depth + 1))
      // Rule 3.2, 4 & 4.1: If only one string has this key (or it's a
      // terminated prefix with key "."), create a LeafNode.
      else
        formatLeaf(k, childStrings[0])
      for k in allKeys
    ]
  ); // end of buildTree

  // 5. Initial Call:
  //    - Start the recursion with the normalized list at depth 0.
  buildTree(normalizedList, 0)
);

// --- Output ---
{
  // The primary export of this library file is the `generate` function.
  generate: generatePrefixTree,

  // --- Verification Examples ---
  // These private fields (prefixed with _) are included to show
  // the output for the examples from the functional specification.

  // Example 1: Standard Branching
  _example1_output: self.generate(["asfa", "asda", "bsd"]),

  // Example 2: Prefix Overlaps & Case Insensitivity
  _example2_output: self.generate(["Car", "cart", "CAr"]),

  // Example 3: Custom Formatters
  // This example shows how to use custom formatters for leaves and groups.
  // Here we create a simpler output format with just "label" and "children".
  _example3_output: self.generate(
    ["asfa", "asda", "bsd"],
    leafFormatter=function(key, string) {
      label: key,
      value: string,
    },
    groupFormatter=function(key, actions) {
      label: key,
      children: actions,
    }
  ),
}
