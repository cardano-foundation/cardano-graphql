{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, criterion, fetchgit, filepath, hedgehog, hpack, prettyprinter
, protolude, regex-tdfa, scientific, stdenv, template-haskell, text
, text-builder, th-lift-instances, unordered-containers, vector
}:
mkDerivation {
  pname = "graphql-parser";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/hasura/graphql-parser-hs.git";
    sha256 = "0srz1f969ipbrm1mk4lr2cj7rf9h17mbimc8pjds8g4x0q9ym6mm";
    rev = "2e8adedbb426d487df77bde244b7fe3cbd40a255";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers filepath hedgehog
    prettyprinter protolude regex-tdfa scientific template-haskell text
    text-builder th-lift-instances unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers filepath hedgehog
    prettyprinter protolude regex-tdfa scientific template-haskell text
    text-builder th-lift-instances unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    aeson attoparsec base bytestring containers criterion filepath
    hedgehog prettyprinter protolude regex-tdfa scientific
    template-haskell text text-builder th-lift-instances
    unordered-containers vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/hasura/graphql-parser-hs#readme";
  license = stdenv.lib.licenses.bsd3;
}
