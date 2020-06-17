{ mkDerivation
, aeson
, aeson-casing
, ansi-wl-pprint
, asn1-encoding
, asn1-types
, async
, attoparsec
, attoparsec-iso8601
, auto-update
, base
, base64-bytestring
, byteorder
, bytestring
, case-insensitive
, ci-info
, containers
, cryptonite
, data-has
, dependent-map
, dependent-sum
, ekg-core
, ekg-json
, fast-logger
, fetchFromGitHub
, file-embed
, filepath
, generic-arbitrary
, ghc-heap-view
, graphql-parser
, hashable
, hspec
, http-client
, http-client-tls
, http-types
, immortal
, insert-ordered-containers
, jose
, lens
, list-t
, mime-types
, monad-control
, monad-time
, monad-validate
, mtl
, mustache
, network
, network-uri
, optparse-applicative
, pem
, pg-client
, postgresql-binary
, postgresql-libpq
, process
, regex-tdfa
, scientific
, semialign
, semver
, shakespeare
, split
, Spock-core
, stdenv
, stm
, stm-containers
, string-conversions
, template-haskell
, text
, text-builder
, text-conversions
, th-lift-instances
, these_0_7_6
, time
, transformers
, transformers-base
, unix
, unordered-containers
, uri-encode
, uuid
, vector
, wai
, wai-websockets
, warp
, websockets
, wreq
, x509
, yaml
, zlib
, sources
}:

mkDerivation {
  pname = "graphql-engine";
  version = "1.2.1";
  src = sources.graphql-engine;
  postUnpack = "sourceRoot+=/server; echo source root reset to $sourceRoot";
  isLibrary = true;
  enableLibraryProfiling = false;
  enableExecutableProfiling = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    aeson-casing
    ansi-wl-pprint
    asn1-encoding
    asn1-types
    async
    attoparsec
    attoparsec-iso8601
    auto-update
    base
    base64-bytestring
    byteorder
    bytestring
    case-insensitive
    ci-info
    containers
    cryptonite
    data-has
    dependent-map
    dependent-sum
    ekg-core
    ekg-json
    fast-logger
    file-embed
    filepath
    generic-arbitrary
    ghc-heap-view
    graphql-parser
    hashable
    http-client
    http-types
    immortal
    insert-ordered-containers
    jose
    lens
    list-t
    mime-types
    monad-control
    monad-time
    monad-validate
    mtl
    mustache
    network
    network-uri
    optparse-applicative
    pem
    pg-client
    postgresql-binary
    postgresql-libpq
    process
    regex-tdfa
    scientific
    semialign
    semver
    shakespeare
    split
    Spock-core
    stm
    stm-containers
    string-conversions
    template-haskell
    text
    text-builder
    text-conversions
    th-lift-instances
    these_0_7_6
    time
    transformers
    transformers-base
    unordered-containers
    uri-encode
    uuid
    vector
    wai
    wai-websockets
    warp
    websockets
    wreq
    x509
    yaml
    zlib
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
