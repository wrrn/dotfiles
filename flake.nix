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
      dotFiles = (
        let
          ignoredFiles = [
            ".git"
            ".gitignore"
            "LICENSE"
            "Makefile"
            "flake.lock"
            "flake.nix"
          ];
          isIgnored = f: builtins.elem f ignoredFiles;
          allFiles = builtins.attrNames (builtins.readDir ./.);
        in
        builtins.filter (p: !isIgnored p) allFiles
      );

      mkDotfile =
        { pkgs, pname }:
        pkgs.stdenv.mkDerivation {
          inherit pname;
          name = pname;
          src = ./${pname};
          installPhase = ''
            mkdir -p $out
            cp -r $src/.* $out/ || true
            cp -r $src/* $out/  || true
          '';
        };

      packageDotfiles =
        { pkgs }: pkgs.lib.attrsets.genAttrs dotFiles (dir: (pkgs.callPackage mkDotfile { pname = dir; }));
    in
    {
      packages = flake-utils.lib.eachSystem flake-utils.lib.allSystems (
        system:
        let
          pkgs = import nixpkgs { system = system; };
        in
        pkgs.callPackage packageDotfiles { }
      );
    };
}
