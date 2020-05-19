{ buildGoPackage
, hasuraHaskellPackages
}:

let
  version = hasuraHaskellPackages.graphql-engine.version;
in
  buildGoPackage rec {
    pname = "hasura-cli";
    inherit version;

    src = hasuraHaskellPackages.graphql-engine.src + "/cli/";

    goPackagePath = "github.com/hasura/graphql-engine/cli/";
    goDeps = ./hasura-cli_deps.nix;

    subPackages = [ "cmd/hasura" ];   # limits the builder from building child packages that have not been listed

    buildFlags = [ "--tags" "release" ];
  }
