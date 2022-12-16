{
  cell,
  inputs,
}: let
  inherit (inputs.nixpkgs) lib;
  inherit (inputs.haskellNix.legacyPackages) haskell-nix;

  graphql-engine-project = import ./graphql-engine-project.nix {
    inherit haskell-nix inputs;
    src = inputs.graphql-engine;
  };

in {
  inherit graphql-engine-project;
}
