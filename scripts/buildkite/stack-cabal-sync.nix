{ system ? builtins.currentSystem }:

let
  pkgs = import ../../nix { inherit system; };
in pkgs.runCommand "stack-cabal-sync-shell" {
  buildInputs = [ pkgs.commonLib.cardano-repo-tool ];
  shellHook = ''
    for EXE in cardano-repo-tool; do
      source <($EXE --bash-completion-script `type -p $EXE`)
    done
  '';
} ""
