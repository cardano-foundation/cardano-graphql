{ mkDerivation, aeson, aeson-casing, ansi-wl-pprint, asn1-encoding
, asn1-types, async, attoparsec, attoparsec-iso8601, auto-update
, base, base64-bytestring, byteorder, bytestring, case-insensitive
, ci-info, containers, cryptonite, data-has, ekg-core, ekg-json
, fast-logger, fetchFromGitHub, file-embed, filepath, graphql-parser
, hashable, hspec, http-client, http-client-tls, http-types
, insert-ordered-containers, jose, lens, list-t, mime-types
, monad-control, monad-time, monad-validate, mtl, mustache, network
, network-uri, optparse-applicative, pem, pg-client
, postgresql-binary, postgresql-libpq, process, regex-tdfa
, scientific, semver, shakespeare, split, Spock-core, stdenv, stm
, stm-containers, string-conversions, template-haskell, text
, text-builder, text-conversions, th-lift-instances, time
, transformers, transformers-base, unix, unordered-containers, uuid
, vector, wai, wai-websockets, warp, websockets, wreq, x509, yaml
, zlib, dependent-map, dependent-sum, generic-arbitrary, these_0_7_6
, ghc-heap-view, immortal, uri-encode
}:

mkDerivation {
  pname = "graphql-engine";
  version = "1.1.1";
  src = fetchFromGitHub {
    owner = "hasura";
    repo = "graphql-engine";
    sha256 = "0v5fs4ma2vxs1bygp45j62jg68bk4skvnf8g9j81b6jydda18lzs";
    rev = "ad07c06e5037f0deb83a2d3ccf1703df6cad1d35";
  };
  postUnpack = "sourceRoot+=/server; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing ansi-wl-pprint asn1-encoding asn1-types async
    attoparsec attoparsec-iso8601 auto-update base base64-bytestring
    byteorder bytestring case-insensitive ci-info containers cryptonite
    data-has ekg-core ekg-json fast-logger file-embed filepath
    graphql-parser hashable http-client http-types
    insert-ordered-containers jose lens list-t mime-types monad-control
    monad-time monad-validate mtl mustache network network-uri
    optparse-applicative pem pg-client postgresql-binary
    postgresql-libpq process regex-tdfa scientific semver shakespeare
    split Spock-core stm stm-containers string-conversions
    template-haskell text text-builder text-conversions
    th-lift-instances time transformers transformers-base
    unordered-containers uuid vector wai wai-websockets warp websockets
    wreq x509 yaml zlib dependent-map dependent-sum generic-arbitrary
    these_0_7_6 uri-encode ghc-heap-view immortal
  ];
  executableHaskellDepends = [
    aeson base bytestring http-client http-client-tls lens mtl
    optparse-applicative pg-client stm string-conversions
    template-haskell text time unix unordered-containers uuid warp wreq
    yaml
  ];
  testHaskellDepends = [
    base hspec http-client http-client-tls optparse-applicative
    pg-client
  ];
  homepage = "https://www.hasura.io";
  description = "GraphQL API over Postgres";
  license = stdenv.lib.licenses.asl20;
}
