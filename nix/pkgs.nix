{ pkgs
, src
, haskellCompiler ? "ghc865"
}:
let

  haskell = pkgs.haskell-nix;

  recRecurseIntoAttrs = with pkgs; pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
  pkgSet = recRecurseIntoAttrs (x: with pkgs; lib.isAttrs x && !lib.isDerivation x)
    # we are only intersted in listing the project packages
    (pkgs.lib.filterAttrs (with pkgs.haskell-nix.haskellLib; (n: p: p != null && (isLocalPackage p && isProjectPackage p) || n == "shellFor"))
      # from our project which is based on a cabal project.
      (pkgs.haskell-nix.cabalProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit { inherit src; };
          ghc = pkgs.buildPackages.haskell-nix.compiler.${haskellCompiler};
        modules = [
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
            }
        ];
      })
    );
 in pkgSet
