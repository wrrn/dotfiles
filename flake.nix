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
      dirEntries = builtins.readDir ./.;
      dotFiles = builtins.filter
        (name: dirEntries.${name} == "directory" && name != ".git")
        (builtins.attrNames dirEntries);

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
