
with import ./. {};

haskell.lib.buildStackProject {
  name = "stack-env";
  buildInputs = with pkgs; [ zlib openssl git gnutar ];
  ghc = (import ../shell.nix { inherit pkgs; }).ghc.baseGhc;
}
