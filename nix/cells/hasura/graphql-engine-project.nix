{
  haskell-nix,
  src,
  inputs,

}: let
  inherit (haskell-nix) haskellLib;
in
  haskell-nix.cabalProject' ({
    config,
    pkgs,
    lib,
    ...
  }: {
    name = "graphql-engine";
    # default-compiler
    compiler-nix-name = lib.mkDefault "ghc925";
    inherit src;
    inputMap = {
      "https://github.com/hasura/kriti-lang.git" = inputs.kriti-lang;
      "https://github.com/hasura/ci-info-hs.git" = inputs.ci-info-hs;
      "https://github.com/hasura/pool.git" = inputs.pool;
      "https://github.com/fpco/odbc.git" = inputs.odbc;
      "https://github.com/hasura/ekg-core.git" = inputs.ekg-core;
      "https://github.com/hasura/ekg-prometheus.git/v0.2.0.0" = inputs.ekg-prometheus;
      "https://github.com/hasura/ekg-json.git" = inputs.ekg-json;
      "https://github.com/hasura/text.git" = inputs.text;
    };
    modules = [{
      packages.mysql.components.library.build-tools = [pkgs.libmysqlclient];
      packages.mysql-simple.components.library.build-tools = [ pkgs.zlib pkgs.openssl_3 ];
      packages.graphql-engine.components.library.preBuild = ''echo 2.16.1 > CURRENT_VERSION'';
      packages.graphql-engine.components.library.build-tools = [ pkgs.zlib pkgs.openssl_3 ];
      packages.graphql-engine.components.exes.graphql-engine.build-tools = [ pkgs.zlib pkgs.openssl_3 ];
      packages.graphql-engine.components.exes.emit-metadata-openapi.build-tools = [ pkgs.zlib pkgs.openssl_3 ];
    }];
  })
