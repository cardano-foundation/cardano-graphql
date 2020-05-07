############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    config = {}; overlays = [];
  };
  graphql-engine = import ./nix/graphql-engine;
  persistgraphql = (import ./nix/node-packages {}).persistgraphql;

in

pkgs.lib.fix (self: {
  inherit ( import ./default.nix ) cardano-graphql;
  inherit graphql-engine persistgraphql;
  required = pkgs.releaseTools.aggregate {
    name = "required";
    constituents = [ self.cardano-graphql self.graphql-engine self.persistgraphql ];
  };
})
