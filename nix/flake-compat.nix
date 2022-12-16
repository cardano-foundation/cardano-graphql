let
  src = ../.;
  lock = builtins.fromJSON (builtins.readFile (src + "/flake.lock"));
  flake-compate-input = lock.nodes.root.inputs.flake-compat;
  nixpkgs-input = lock.nodes.root.inputs.nixpkgs;
  flake-compat = import (builtins.fetchTree lock.nodes.${flake-compate-input}.locked);
  pkgs = import (builtins.fetchTree lock.nodes.${nixpkgs-input}.locked) { };
in
flake-compat {
  inherit src pkgs;
}
