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

in

pkgs.lib.fix (self: {
  inherit ( import ./default.nix ) cardano-graphql;
  required = pkgs.releaseTools.aggregate {
    name = "required";
    constituents = [ self.cardano-graphql ];
  };
})
