{ pkgs, lib }:

let
  stdFlake = (import ./flake-compat.nix).defaultNix.${pkgs.system};
  hasuraPackages = stdFlake.hasura.packages;
  packages = self: hasuraPackages // {
    inherit (stdFlake.cardano-graphql.packages) cardano-graphql persistgraphql;
    vgo2nix = pkgs.vgo2nix;
  };
in lib.makeScope pkgs.newScope packages
