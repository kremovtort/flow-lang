{
  description = "Haskell development environment";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  
  outputs = { self, nixpkgs, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];
      
      perSystem = { system, pkgs, ... }: let
        haskellPackages = pkgs.haskell.packages.ghc912;
      in {
        devShells.default = pkgs.mkShell {
          packages = [
            haskellPackages.ghc
            haskellPackages.cabal-gild
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.fourmolu
            pkgs.zlib
          ];
        };
      };
    };
}
