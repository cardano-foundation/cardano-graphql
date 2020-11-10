{ pkgs, lib }:

let
  packages = self: {
    nix-inclusive = pkgs.callPackage "${pkgs.sources.nix-inclusive}/inclusive.nix" {};
    cardano-graphql = self.callPackage ./cardano-graphql.nix {};
    hasura-cli-ext = self.callPackage ./hasura-cli-ext/impure.nix {};
    persistgraphql = (self.callPackage ./persistgraphql {}).persistgraphql;
    hasura-cli = pkgs.hasura-cli.overrideAttrs (_: rec {
      src = pkgs.sources.graphql-engine;
      name = "hasura-${version}";
      version = "1.3.2";
      buildFlagsArray = [''-ldflags=
        -X github.com/hasura/graphql-engine/cli/version.BuildVersion=${version}
        -s
        -w
      ''];
    });
    vgo2nix = pkgs.vgo2nix;
    hasuraHaskellPackages = pkgs.haskell.packages.ghc8102.override {
      overrides = import ./graphql-engine/hs-overlay.nix { inherit (pkgs) haskell sources postgresql openssl; };
    };
    graphql-engine = pkgs.haskell.lib.justStaticExecutables self.hasuraHaskellPackages.graphql-engine;
  };
in lib.makeScope pkgs.newScope packages
