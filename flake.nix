{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils/main";
  };

  #   outputs = {self, nixpkgs, flake-utils, ... }@inputs:
  #     flake-utils.lib.eachSystem flake-utils.lib.allSystems (system: {hello = "name";});
  # }

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (
      system:
      let
        pkgs = import nixpkgs { system = "aarch64-darwin"; };
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
          pname:
          pkgs.stdenv.mkDerivation {
            pname = "${pname}-conf";
            version = "0.0.1";
            src = ./${pname};
            installPhase = ''
              mkdir -p $out
              cp -r $src/.* $out/ || true
              cp -r $src/* $out/  || true
            '';
          };

      in
      {
        packages = pkgs.lib.foldr (
          dotfile: attrSet: attrSet // { "${dotfile}" = mkDotfile dotfile; }
        ) { } dotFiles;
      }
    );
}
