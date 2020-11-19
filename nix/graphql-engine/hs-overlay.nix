{ haskell, sources, postgresql, openssl }:

self: super: {
  ci-info = haskell.lib.dontHaddock (haskell.lib.dontCheck (super.ci-info.overrideAttrs (_: {
    src = sources.ci-info-hs;
  })));
  graphql-engine = haskell.lib.dontHaddock (haskell.lib.dontCheck (super.graphql-engine.overrideAttrs (_: {
    src = sources.graphql-engine;
    name = "graphql-engine-1.3.3";
  })));
  graphql-parser = haskell.lib.dontHaddock (haskell.lib.dontCheck (super.graphql-parser.overrideAttrs (_: {
    src = sources.graphql-parser-hs;
    prePatch = "hpack --force";
  })));
  pg-client = haskell.lib.dontHaddock (haskell.lib.dontCheck (super.pg-client.overrideAttrs (attrs: {
    src = sources.pg-client-hs;
    nativeBuildInputs = attrs.nativeBuildInputs ++ [ postgresql ];
    buildInputs = attrs.buildInputs ++ [ openssl ];
  })));
}
