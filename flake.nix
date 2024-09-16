{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };
  outputs = {self, nixpkgs, lib, ... }@inputs :{
    packages.amethyst = lib.mkDerivation {
      pname = "amethyst-conf";
      version = "0.0.1";
      src = ./amethyst;        
    };
    
# xxxx    pakages.wezterm = mkDerivation {
      
#     };

#     packages.emacs = mkDerivation {

#     };

#     packages.fish = mkDerivation {
      
#     };

#     packages.git = mkDerivation {

#     };

#     packages.starship = mkDerivation {

#     };

#     packages.tridactyl = mkDerivation {
      
#     };

#     packages.wezterm = mkDerivation {

#     };            
  };
}
