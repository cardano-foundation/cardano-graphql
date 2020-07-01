############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################
{
  cardano-graphql ? { rev = null; }
}:

let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix {};
in

pkgs.lib.fix (self: {
  inherit ( import ./. ) cardano-graphql hasura-cli hasura-cli-ext persistgraphql graphql-engine;
  build-version = pkgs.writeText "version.json" (builtins.toJSON { inherit (cardano-graphql) rev; });
  required = pkgs.releaseTools.aggregate {
    name = "required";
    constituents = with self; [
      self.cardano-graphql
      graphql-engine
      hasura-cli
      hasura-cli-ext
      persistgraphql
      build-version
    ];
  };
})
