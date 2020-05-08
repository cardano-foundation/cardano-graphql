############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################
{
  rev ? null
}:

let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix {};
  graphql-engine = import ./nix/graphql-engine;
in

pkgs.lib.fix (self: {
  inherit ( import ./default.nix ) cardano-graphql persistgraphql;
  inherit graphql-engine;
  build-version = pkgs.writeText "version.json" (builtins.toJSON { inherit rev; });
  required = pkgs.releaseTools.aggregate {
    name = "required";
    constituents = with self; [ cardano-graphql graphql-engine persistgraphql build-version ];
  };
})
