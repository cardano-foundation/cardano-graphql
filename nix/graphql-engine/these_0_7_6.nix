{ stdenv, mkDerivation, aeson, base, base-compat, bifunctors, binary
, containers, data-default-class, deepseq, hashable, keys, lens
, mtl, QuickCheck, quickcheck-instances, semigroupoids, tasty
, tasty-quickcheck, transformers, transformers-compat
, unordered-containers, vector, vector-instances
}:
mkDerivation {
  pname = "these";
  version = "0.7.6";
  sha256 = "0in77b1g73m224dmpfc9khgcs0ajgsknp0yri853c9p6k0yvhr4l";
  libraryHaskellDepends = [
    aeson base base-compat bifunctors binary containers
    data-default-class deepseq hashable keys lens mtl QuickCheck
    semigroupoids transformers transformers-compat unordered-containers
    vector vector-instances
  ];
  testHaskellDepends = [
    aeson base base-compat bifunctors binary containers hashable lens
    QuickCheck quickcheck-instances tasty tasty-quickcheck transformers
    unordered-containers vector
  ];
  description = "An either-or-both data type & a generalized 'zip with padding' typeclass";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
