{ pkgs
, sources
}:
let
  stateDir = "./state-cluster";
  iohkNix = import sources.iohk-nix {};
  scriptPath = pkgs.lib.makeBinPath (with pkgs; [ coreutils postgresql ]);
  postgresScript = pkgs.writeScript "run-postgresql" ''
    #!${pkgs.stdenv.shell}
    set -x
    export PATH=${scriptPath}
    rm -rf postgres-dir
    initdb --encoding=UTF8 --locale=en_US.UTF-8 --username=$USER postgres-dir
    postgres -D postgres-dir -p 30100 -k ./postgres-dir &
    PSQL_PID=$!
    sleep 10
    if (echo '\q' | psql -h ./postgres-dir $USER postgres); then
      echo "PostgreSQL server is verified to be started."
    else
      echo "Failed to connect to local PostgreSQL server."
      exit 2
    fi
  '';
  dbSyncScript = let
    config = __toFile "config.json" (__toJSON (iohkNix.cardanoLib.defaultLogConfig // {
      EnableLogMetrics = false;
      EnableLogging = true;
      GenesisHash = "";
      NetworkName = "local_cluster";
      RequiresNetworkMagic = "RequiresMagic";
    }));
    schema = sources.cardano-db-sync + "/schema";
    exec = (import sources.cardano-db-sync {}).cardano-db-sync-extended;
    script = pkgs.writeScript "run-db-sync" ''
      #!${pkgs.stdenv.shell}
      export PATH=${scriptPath}
      export PGPASSFILE=pgpass
      export DBUSER=sam
      export DBNAME=cardano-db
      echo "./postgres-dir:30100:$DBUSER:$DBNAME:*" > $PGPASSFILE
      chmod 0600 $PGPASSFILE
      sleep 10
      exec ${exec}/bin/cardano-db-sync-extended \
             --config ${config} \
             --genesis-file keys/genesis.json \
             --socket-path "$CARDANO_NODE_SOCKET_PATH" \
             --schema-dir ${schema}
    '';
  in script;
  cardanoNodeCluster = let
    extraSupervisorConfig = {
      "program:cardano-db-sync" = {
        command = "${dbSyncScript}";
        autorestart = true;
        stdout_logfile = "${stateDir}/db-sync.stdout";
        stderr_logfile = "${stateDir}/db-sync.stderr";
        directory = stateDir;
      };
      "program:postgresql" = {
        command = "${postgresScript}";
        autorestart = true;
        stdout_logfile = "${stateDir}/postgres.stdout";
        stderr_logfile = "${stateDir}/postgres.stderr";
        directory = stateDir;
      };
    };
    customConfig = {
      inherit extraSupervisorConfig stateDir;
    };
  in (import sources.cardano-node { inherit customConfig; }).cluster;

in {
  inherit (cardanoNodeCluster) start stop baseEnvConfig;
}
