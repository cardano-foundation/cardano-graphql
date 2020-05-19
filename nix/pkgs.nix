{ sources ? import ./sources.nix }:

let
  overlay = self: super: {
    packages = self.callPackages ./packages.nix {};
  };
in
  import sources.nixpkgs { overlays = [ overlay ] ; config = {}; }
