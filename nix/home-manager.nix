# Links dotfiles packages into $HOME, one switch per package:
#
#   wrrn.dotfiles.fish.enable = true;
#
# A package's links mirror its stow layout: fish/dot-config/fish/config.fish
# becomes ~/.config/fish/config.fish. Each file gets its own link, like
# `stow --no-folding`; list directories in `fold` to link them whole.
#
# `source` picks what the links point at:
# - "store": this flake's store copy. Read-only; updated with
#   `nix flake update dotfiles` and a rebuild.
# - "localPath": the working clone at `wrrn.dotfiles.localPath`, so edits
#   take effect without a rebuild.
#
# In both modes the file list comes from this flake's store copy, because pure
# evaluation can't read the clone. A new file is linked after
# `nix flake update dotfiles` and a rebuild, unless it's under a folded
# directory.
#
# See design-log/2026-11-11-home-manager-module.md.
self:
{ config, lib, ... }:
let
  inherit (lib) mkOption types;

  cfg = config.wrrn.dotfiles;
  src = self.outPath;
  layout = import ./packages.nix;

  globalOptions = [ "localPath" ];

  packages = lib.filter (
    name: !lib.hasPrefix "." name && !lib.elem name (layout.notPackages ++ layout.notLinkable)
  ) (lib.attrNames (lib.filterAttrs (_: type: type == "directory") (builtins.readDir src)));

  # stow --dotfiles: a leading "dot-" in any path component means ".".
  targetName = name: if lib.hasPrefix "dot-" name then ".${lib.removePrefix "dot-" name}" else name;

  join = dir: name: if dir == "" then name else "${dir}/${name}";

  # [ { path = "dot-config/fish/config.fish"; target = ".config/fish/config.fish"; } ]
  # Stops at directories listed in `fold`, returning the directory itself.
  walk =
    pkg: fold: path: target:
    lib.concatLists (
      lib.mapAttrsToList (
        name: type:
        let
          entry = {
            path = join path name;
            target = join target (targetName name);
          };
        in
        if type == "directory" && !lib.elem entry.target fold then
          walk pkg fold entry.path entry.target
        else
          [ entry ]
      ) (builtins.readDir "${src}/${pkg}${lib.optionalString (path != "") "/${path}"}")
    );

  foldOf = pkg: map (lib.removeSuffix "/") cfg.${pkg}.fold;
  entriesOf = pkg: walk pkg (foldOf pkg) "" "";

  sourceOf =
    pkg: path:
    if cfg.${pkg}.source == "store" then
      "${src}/${pkg}/${path}"
    else if cfg.localPath == null then
      throw "wrrn.dotfiles.${pkg}.source is \"localPath\" but wrrn.dotfiles.localPath is not set"
    else
      config.lib.file.mkOutOfStoreSymlink "${cfg.localPath}/${pkg}/${path}";

  enabled = lib.filter (pkg: cfg.${pkg}.enable) packages;
  usesLocalPath = lib.filter (pkg: cfg.${pkg}.source == "localPath") enabled;

  packageOptions = pkg: {
    enable = lib.mkEnableOption "the ${pkg} dotfiles";

    source = mkOption {
      type = types.enum [
        "store"
        "localPath"
      ];
      default = if cfg.localPath == null then "store" else "localPath";
      defaultText = lib.literalExpression ''
        if config.wrrn.dotfiles.localPath == null then "store" else "localPath"
      '';
      description = ''
        What the ${pkg} links point at: "store" for this flake's read-only
        store copy, "localPath" for the working clone at
        `wrrn.dotfiles.localPath` (edits take effect without a rebuild).
      '';
    };

    fold = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [ ".config/${pkg}" ];
      description = ''
        Directories, as paths relative to the home directory, to link whole
        instead of file by file. With source "localPath", new files under a
        folded directory appear without a rebuild, and files the program
        creates there land in the repo.
      '';
    };
  };
in
{
  options.wrrn.dotfiles = {
    localPath = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = lib.literalExpression ''"''${config.home.homeDirectory}/burrow/dotfiles"'';
      description = ''
        Absolute path to a working clone of dotfiles. Packages use it when
        their `source` is "localPath", the default when this is set.
      '';
    };
  }
  // lib.genAttrs packages (
    pkg:
    if lib.elem pkg globalOptions then
      throw "dotfiles: package ${pkg} clashes with the global option wrrn.dotfiles.${pkg}; rename the directory"
    else
      packageOptions pkg
  );

  config = lib.mkIf (enabled != [ ]) {
    assertions = lib.concatMap (
      pkg:
      let
        targets = map (entry: entry.target) (entriesOf pkg);
      in
      [
        {
          assertion = cfg.${pkg}.source != "localPath" || cfg.localPath != null;
          message = "wrrn.dotfiles.${pkg}.source is \"localPath\" but wrrn.dotfiles.localPath is not set.";
        }
      ]
      ++ map (dir: {
        assertion = lib.elem dir targets;
        message = "wrrn.dotfiles.${pkg}.fold: ${dir} is not a directory in the ${pkg} package.";
      }) (foldOf pkg)
    ) enabled;

    home.file = lib.listToAttrs (
      lib.concatMap (
        pkg:
        map (entry: {
          name = "dotfiles-${pkg}:${entry.target}";
          value = {
            source = sourceOf pkg entry.path;
            inherit (entry) target;
            # Replace links left by `make <pkg>.stow`.
            force = true;
          };
        }) (entriesOf pkg)
      ) enabled
    );

    home.activation.dotfilesLocalPath = lib.mkIf (usesLocalPath != [ ] && cfg.localPath != null) (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -d ${lib.escapeShellArg cfg.localPath} ]; then
          echo "dotfiles: ${cfg.localPath} is missing, so the links for ${lib.concatStringsSep ", " usesLocalPath} dangle; clone git@git.sr.ht:~warren/dotfiles there" >&2
        fi
      ''
    );
  };
}
