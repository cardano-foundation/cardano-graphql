# our packages overlay
pkgs: _: with pkgs; {
  cardanoPreludeHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };

}
