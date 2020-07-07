{ haskell, sources }:

self: super: {
  ci-info = self.callPackage ./ci-info.nix {};
  graphql-engine = haskell.lib.dontHaddock (haskell.lib.dontCheck (self.callPackage ./graphql-engine.nix { inherit sources; }));
  graphql-parser = self.callPackage ./graphql-parser.nix {};
  pg-client = self.callPackage ./pg-client.nix {};
  network-uri = self.callPackage ./network-uri.nix { };
  regex-tdfa = self.callHackageDirect { pkg = "regex-tdfa"; ver = "1.3.1.0"; sha256 = "1a0l7kdjzp98smfp969mgkwrz60ph24xy0kh2dajnymnr8vd7b8g"; } {};
  regex-base = self.callHackageDirect { pkg = "regex-base"; ver = "0.94.0.0"; sha256 = "0x2ip8kn3sv599r7yc9dmdx7hgh5x632m45ga99ib5rnbn6kvn8x"; } {};
  regex-posix = self.callHackageDirect { pkg = "regex-posix"; ver = "0.96.0.0"; sha256 = "0js977ahpz10642sbpb55mw9h01pilai6z201wgkncgkg2d69hl3"; } {};
  regex-compat = self.callHackageDirect { pkg = "regex-compat"; ver = "0.95.2.0"; sha256 = "1hi6k03pqd1kprh1fxg04ffz3gaqrh3dlbficlim54wz3dk576da"; } {};
  shakespeare = self.callHackageDirect { pkg = "shakespeare"; ver = "2.0.22"; sha256 = "1d7byyrc2adyxrgcrlxyyffpr4wjcgcnvdb8916ad6qpqjhqxx72"; } {};
  stm-hamt = haskell.lib.doJailbreak (haskell.lib.unmarkBroken super.stm-hamt);
  ghc-heap-view = self.callPackage ./ghc-heap-view.nix {};
  immortal = self.callPackage ./immortal.nix {};
  superbuffer = haskell.lib.doJailbreak (haskell.lib.unmarkBroken super.superbuffer);
  Spock-core = haskell.lib.unmarkBroken super.Spock-core;
  stm-containers = haskell.lib.unmarkBroken super.stm-containers;
}
