# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, pkgs ? import ./nix/pkgs.nix {}
}:

with pkgs;
let
  # This provides a development environment that can be used with nix-shell
  shell = pkgs.mkShell {
    name = "cardano-graphql-dev-shell";

    # These programs will be available inside the nix-shell.
    buildInputs = [
      git                    # Distributed version control system
      go                     # The Go Programming language
      packages.niv           # Dependency management for Nix projects
      nix                    # Purely Functional Package Manager
      nodePackages.node2nix  # Generates a set of Nix expressions from a NPM package's package.json
      packages.nodejs        # Event-driven I/O framework for the V8 JavaScript engine
      packages.cardano-graphql
      pkgconfig              # Allows packages to find out information about other packages
      pkgs.packages.cardano-cli
      pkgs.packages.persistgraphql
      pkgs.packages.hasura-cli
      python                 # The Python Programming language
      tmux                   # Terminal multiplexer
      yarn                   # Dependency management for javascript
      packages.vgo2nix       # Convert go.mod files to nixpkgs buildGoPackage compatible deps.nix files
      yarn2nix-moretea.yarn2nix   # Converts yarn.lock files into nix expression
    ];

    shellHook = ''
      source <(hasura completion bash)
      echo "Cardano Graphql Dev Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package
        * persistgraphql <src> whitelist.json - generates a whitelist.json to limit graphql queries
        * export GOPATH="\$\(pwd\)/.go" - enable vgo2nix to use the pwd as it's source
        * node2nix -l - update node packages, -l if there's a lock file

      "
    '';
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv                         # Dependency management for Nix projects
      nodePackages.node2nix       # Generates a set of Nix expressions from a NPM package's package.json
      packages.vgo2nix            # Convert go.mod files to nixpkgs buildGoPackage compatible deps.nix files
      yarn2nix-moretea.yarn2nix   # Converts yarn.lock files into nix expression
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package
        * export GOPATH="\$\(pwd\)/.go" - enable vgo2nix to use the pwd as it's source
        * node2nix -l - update node packages, -l if there's a lock file

      "
    '';
  };

in

  shell // { inherit devops; }
