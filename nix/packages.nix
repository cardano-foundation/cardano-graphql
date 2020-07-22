{ pkgs, lib }:

let
  packages = self: {
    sources = import ./sources.nix;
    nodejs = pkgs.nodejs-12_x;
    inherit (import self.sources.niv {}) niv;
    nix-inclusive = pkgs.callPackage "${self.sources.nix-inclusive}/inclusive.nix" {};
    inherit (import self.sources.cardano-node {}) cardano-cli;
    cardano-graphql = self.callPackage ./cardano-graphql.nix {};
    hasura-cli-ext = self.callPackage ./hasura-cli-ext/impure.nix {};
    persistgraphql = (self.callPackage ./persistgraphql {}).persistgraphql;
    hasura-cli = self.callPackage ./hasura-cli {};
    vgo2nix = self.callPackage self.sources.vgo2nix {};
    hasuraHaskellPackages = pkgs.haskellPackages.override {
      overrides = import ./graphql-engine/hs-overlay.nix { inherit (pkgs) haskell; inherit (self) sources; };
    };
    graphql-engine = self.hasuraHaskellPackages.graphql-engine;
  };
in lib.makeScope pkgs.newScope packages
