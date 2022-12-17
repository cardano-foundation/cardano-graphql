{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (cell) packages;
  inherit (inputs.cells) hasura;
  inherit (hasura.packages) hasura-cli-ext;
  installHasuraCLI = ''
    # always start with no plugins so future upgrades will work
    rm -rf ~/.hasura/plugins
    mkdir -p ~/.hasura/plugins/store/cli-ext/v${hasura-cli-ext.version}
    ln -s ${hasura-cli-ext}/bin/cli-ext-hasura-linux ~/.hasura/plugins/store/cli-ext/v${hasura-cli-ext.version}/cli-ext-hasura-linux
  '';
  packages' = packages;
in nixpkgs.lib.makeOverridable ({ evalSystem ? throw "unreachable" }@args: let
  packages = if args ? evalSystem then packages'.override { inherit evalSystem; } else packages';

  prelude-runtime = with nixpkgs; [coreutils netcat curl glibc.bin patchelf];
in {
  cardano-graphql = writeShellApplication {
    runtimeInputs = prelude-runtime;
    name = "entrypoint";
    text = ''
      exec ${packages.cardano-graphql}/bin/cardano-graphql
    '';
  };
  cardano-graphql-background = writeShellApplication {
    runtimeInputs = prelude-runtime ++ [
      packages.persistgraphql
      hasura.packages.hasura-cli
      hasura.packages.hasura-cli-ext
    ];
    name = "entrypoint";
    text = ''
      set -exuo pipefail
      ${installHasuraCLI}
      exec ${packages.cardano-graphql}/bin/cardano-graphql-background
    '';
  };
}) {}
