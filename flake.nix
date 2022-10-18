{
  description = "dumbcc";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, flake-utils, nixpkgs, haskellNix, ... }@inputs:
    {
      overlay = (final: prev: {
        dumbcc' = final.haskell-nix.project' {
          src = ./.;
          compiler-nix-name = "ghc924";
        };

        dumbcc = (final.dumbcc'.flake
          { }).packages."dumbcc:exe:dumbcc";
      });
    } // (flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay self.overlay ];
        };
        lib = pkgs.lib;
      in rec {
        packages = { inherit (pkgs) dumbcc; };

        devShells.default = pkgs.dumbcc'.shellFor {
          tools = {
            cabal = "latest";
            hlint =
              "latest"; # Selects the latest version in the hackage.nix snapshot
            hindent = "latest";
            haskell-language-server = "latest";
            hpack = "latest";
          };

          LD_LIBRARY_PATH = lib.makeLibraryPath [ ];
        };
      }));
}
