{
  cell,
  inputs,
}:

import "${inputs.self}/release.nix" {
  cardano-graphql = inputs.self;
  inherit (inputs.nixpkgs) system;
}
