{ pkgs, lib }:

let
  hasuraPackages = (import ./flake-compat.nix).defaultNix.${pkgs.system}.hasura.packages;
  packages = self: hasuraPackages // {
    nix-inclusive = pkgs.callPackage "${pkgs.sources.nix-inclusive}/inclusive.nix" {};
    cardano-graphql = self.callPackage ./cardano-graphql.nix {};
    persistgraphql = (self.callPackage ./persistgraphql { nodejs = pkgs.nodejs-14_x; }).persistgraphql;
    vgo2nix = pkgs.vgo2nix;
  };
in lib.makeScope pkgs.newScope packages
