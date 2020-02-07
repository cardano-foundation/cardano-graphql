{ system ? builtins.currentSystem
, config ? {}
, pkgs ? import ../../nix { inherit system config; }
, buildTools ? with pkgs; [ git nix gnumake ]
}:

with pkgs.lib;
with pkgs;

let
  inherit (iohkNix) cache-s3;

  stackRebuild = runCommand "stack-rebuild" {} ''
    ${haskellPackages.ghcWithPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath ([ cache-s3 stack gnused coreutils gnutar ] ++ buildTools)}
    exec ${stackRebuild} "$@"
  ''
