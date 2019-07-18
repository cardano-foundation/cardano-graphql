let
  lib = (import ./lib.nix).pkgs.lib;
in {
  target ? builtins.currentSystem
}:

# Generated targets include anything from stack.yaml (via nix-tools:stack-to-nix and the nix/regenerate.sh script)
# or cabal.project (via nix-tools:plan-to-nix), including all
# version overrides specified there.
#
# Nix-tools stack-to-nix will generate the `nix/.stack-pkgs.nix`
# file which is imported from the `nix/pkgs.nix` where further
# customizations outside of the ones in stack.yaml/cabal.project
# can be specified as needed for nix/ci.
#
# Please run `nix/regenerate.sh` after modifying stack.yaml
# or relevant part of cabal configuration files.
# When switching to recent stackage or hackage package version,
# you might also need to update the iohk-nix common lib. You
# can do so by running the `nix/update-iohk-nix.sh` script.
#
# More information about iohk-nix and nix-tools is available at:
# https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix-toolification.org#for-a-stackage-project

let
  system = if target != "x86_64-windows" then target else builtins.currentSystem;
  crossSystem = if target == "x86_64-windows" then lib.systems.examples.mingwW64 else null;
  # commonLib provides iohk-nix tooling and extra libraries specific to cardano-sl.
  commonLib = import ./lib.nix;
  # nixTools contains all the haskell binaries and libraries built by haskell.nix
  nixTools = import ./nix/nix-tools.nix {};
  # scripts contains startup scripts for proxy
in {
  inherit (nixTools) nix-tools;
}
