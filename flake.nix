{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils/main";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    let
      layout = import ./nix/packages.nix;
      dirEntries = builtins.readDir ./.;
      dotFiles = builtins.filter (
        name:
        dirEntries.${name} == "directory"
        && builtins.substring 0 1 name != "."
        && !builtins.elem name layout.notPackages
      ) (builtins.attrNames dirEntries);

      mkDotfiles = pkgs: pkgs.lib.genAttrs dotFiles (pname:
        pkgs.stdenv.mkDerivation {
          inherit pname;
          name = pname;
          src = ./${pname};
          installPhase = ''
            mkdir -p $out
            cp -r $src/.* $out/ || true
            cp -r $src/* $out/  || true
          '';
        }
      );
    in
    {
      # Home Manager module that links packages into $HOME, one
      # wrrn.dotfiles.<package>.enable per package. See nix/home-manager.nix.
      homeManagerModules.default = import ./nix/home-manager.nix self;
      homeModules.default = self.homeManagerModules.default;

      overlays.default = final: prev: {
        dotfiles = mkDotfiles final;
      };
    } // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages = mkDotfiles pkgs;
      }
    );
}
