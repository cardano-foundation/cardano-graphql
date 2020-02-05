{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, commonLib ? import ./lib.nix { inherit system crossSystem config; }
, pkgs ? commonLib.pkgs
, gitrev ? commonLib.iohkNix.commitIdFromGitRepoOrZero ./.git
}:

let
  lib = commonLib.pkgs.lib;
  inherit (commonLib) haskellPackages;

  self = with commonLib; {
    inherit haskellPackages;
    inherit (commonLib.iohkNix) check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isCardanoPrelude haskellPackages;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (filterCardanoPackages haskellPackages));
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isCardanoPrelude haskellPackages;

  };

in self
