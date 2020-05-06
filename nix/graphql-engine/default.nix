let
  sources = import ../sources.nix;
  nixpkgsSrc = sources.nixpkgs-hasura-base;                              
  inherit (import nixpkgsSrc {}) pkgs;
  haskellPackages = pkgs.callPackage ./pkgs.nix {};
  graphqlEngine = haskellPackages.graphql-engine;
in graphqlEngine // { inherit haskellPackages; }
