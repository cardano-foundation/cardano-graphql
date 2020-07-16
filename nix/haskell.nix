############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc865"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-prelude-src";
      src = ../.;
  };

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject  (lib.optionalAttrs stdenv.hostPlatform.isWindows {
    # FIXME: without this deprecated attribute, db-converter fails to compile directory with:
    # Encountered missing dependencies: unix >=2.5.1 && <2.9
    ghc = buildPackages.haskell-nix.compiler.${compiler};
  } // {
    inherit src;
    compiler-nix-name = compiler;
    #ghc = buildPackages.haskell-nix.compiler.${compiler};
    modules = [
      { compiler.nix-name = compiler; }
      # Allow reinstallation of Win32
      { nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      }
      {
          packages.cardano-prelude.configureFlags = [ "--ghc-option=-Werror" ];
          enableLibraryProfiling = profiling;
      }
    ];
  });
in
  pkgSet
