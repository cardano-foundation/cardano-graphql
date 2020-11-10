############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################
{ cardano-graphql ? { rev = null; }
, pkgs ? import ./nix/pkgs.nix {}
}:
with pkgs;
let
  mkPins = inputs: runCommand "ifd-pins" {} ''
    mkdir $out
    cd $out
    ${lib.concatMapStringsSep "\n" (input: "ln -sv ${input.value} ${input.key}") (lib.attrValues (lib.mapAttrs (key: value: { inherit key value; }) inputs))}
  '';
in

lib.fix (self: {
  ifd-pins = mkPins {
      inherit (sources) ci-info-hs graphql-engine graphql-parser-hs pg-client-hs;
  };
  inherit (packages) cardano-graphql hasura-cli hasura-cli-ext persistgraphql graphql-engine;
  build-version = writeText "version.json" (builtins.toJSON { inherit (cardano-graphql) rev; });
  required = releaseTools.aggregate {
    name = "required";
    constituents = with self; [
      cardano-graphql
      graphql-engine
      hasura-cli
      hasura-cli-ext
      persistgraphql
      build-version
    ];
  };
})
