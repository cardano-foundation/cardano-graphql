{ sources ? import ./sources.nix }:
let
  # TODO: filter src to just the files needed to build
  src = ../.;
in with
  { overlay = self: super:
      { inherit (import sources.niv {}) niv;
        packages = self.callPackages ./packages.nix { cardano-graphql-src = src; };
        node = super.nodejs-12_x;
        inherit (import sources.yarn2nix { pkgs = self; }) yarn2nix mkYarnModules mkYarnPackage;
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
