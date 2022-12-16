{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  inherit (cell.library.graphql-engine-project.hsPkgs.graphql-engine.components.exes) graphql-engine emit-metadata-openapi;
  inherit (inputs.graphql-engine.packages) graphql-parser;
  hasura-cli = nixpkgs.callPackage ./cli.nix {
    src = inputs.graphql-engine;
  };
  hasura-cli-ext = nixpkgs.callPackage ./cli-ext.nix {};
}
