{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:

let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  # use our own nixpkgs if it exists in our sources,
  # otherwise use iohkNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then (builtins.trace "Not using IOHK default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else (import sources.iohk-nix {}).nixpkgs;

  nixpkgs2205 = import sources."nixpkgs-22.05" {};

  overlay = self: super: {
    packages = self.callPackages ./packages.nix { };
    #nodejs = super.nodejs-14_x;
    nodejs = nixpkgs2205.nodejs-16_x;
    inherit sources;
  };
  pkgs = import nixpkgs {
    inherit system crossSystem config;
    overlays = [ overlay ];
  };
in
  pkgs
