let pkgs = import ./nix/pkgs.nix {};
    persistgraphql = (import ./nix/node-packages {}).persistgraphql;

in pkgs.packages // { inherit persistgraphql; }
