# dotfiles

Each top-level directory is a [GNU Stow](https://www.gnu.org/software/stow/) package whose layout mirrors `$HOME`. A `dot-` prefix stands for `.`, so `fish/dot-config/fish/config.fish` is `~/.config/fish/config.fish`.

`firefox`, `brew` and `bettertouchtool` don't mirror `$HOME`. Tools read their files directly.

## Install

### With Home Manager (normal)

The flake exports a Home Manager module, `homeManagerModules.default`, with one switch per package:

```nix
# flake.nix
inputs.dotfiles.url = "sourcehut:~warren/dotfiles/main";

# home-manager config
imports = [ inputs.dotfiles.homeManagerModules.default ];
wrrn.dotfiles = {
  # Optional: link to a working clone instead of the read-only store copy.
  localPath = "${config.home.homeDirectory}/burrow/dotfiles";

  fish.enable = true;
  emacs.enable = true;
  niri = {
    enable = true;
    source = "store"; # pin this package to flake.lock
  };
};
```

Each package has two options:

| Option | Default | What it does |
|---|---|---|
| `enable` | `false` | Link the package into `$HOME`. |
| `source` | `"localPath"` if `localPath` is set, else `"store"` | `"store"` links to the flake's store copy: changes need a push, `nix flake update dotfiles` and a rebuild. `"localPath"` links to the clone at `localPath`, so edits take effect without a rebuild. |

Every file gets its own link, like `stow --no-folding`, so programs can keep their own files next to yours. The file list comes from the flake's store copy, even with `localPath`: a new file is linked after `nix flake update dotfiles` and a rebuild. Clone the repo yourself; activation warns if `localPath` is missing.

`~/nixos-config` uses the module with `localPath` set. Options are in `nix/home-manager.nix`; the design is in `design-log/2026-11-11-home-manager-module.md`.

### Without Nix (fallback)

```sh
make fish.stow          # link a package
make fish.stow.force    # delete conflicting files first, then link
make fish.unstow        # remove the links
```

Don't stow a package that the Home Manager module manages on the same machine.

## Leader Key

The Leader Key config is generated using jsonnet.
