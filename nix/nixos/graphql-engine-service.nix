{ lib, pkgs, config, ... }:
let
  cfg = config.services.graphql-engine;

in {
  options = {
    services.graphql-engine = {
      enable = lib.mkEnableOption "graphql engine service";

      package = lib.mkOption {
        type = lib.types.package;
        default = (import ../pkgs.nix {}).packages.graphql-engine;
      };

      host = lib.mkOption {
        type = lib.types.str;
        default = "";
      };

      dbUser = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
      };

      password = lib.mkOption {
        type = lib.types.str;
        default = "";
      };

      dbAdminUser = lib.mkOption {
        type = lib.types.str;
        default = "postgres";
      };

      db = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
      };

      dbPort = lib.mkOption {
        type = lib.types.int;
        default = 5432;
      };

      enginePort = lib.mkOption {
        type = lib.types.int;
        default = 9999;
      };
    };
  };
  config = let
    hasuraDbPerms = pkgs.writeScript "hasuraDbPerms.sql" ''
      CREATE EXTENSION IF NOT EXISTS pgcrypto;
      CREATE SCHEMA IF NOT EXISTS hdb_catalog;
      CREATE SCHEMA IF NOT EXISTS hdb_views;
      ALTER SCHEMA hdb_catalog OWNER TO ${cfg.dbUser};
      ALTER SCHEMA hdb_views OWNER TO ${cfg.dbUser};
      GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO ${cfg.dbUser};
      GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO ${cfg.dbUser};
    '';
    postgresqlIp = if (cfg.host == "" || (__head (pkgs.lib.stringToCharacters cfg.host)) == "/")
                   then "127.0.0.1"
                   else cfg.host;
  in lib.mkIf cfg.enable {
    systemd.services.graphql-engine = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      path = with pkgs; [ curl netcat postgresql sudo ];
      environment = {
        HASURA_GRAPHQL_DATABASE_URL = "postgres://${cfg.dbUser}:${cfg.password}@${cfg.host}${if cfg.host == "" then "" else toString ":${toString cfg.dbPort}"}/${cfg.db}";
        CARDANO_GRAPHQL_DB_URL = "postgres://${cfg.dbUser}:${cfg.password}@${cfg.host}${if cfg.host == "" then "" else ":${toString cfg.dbPort}"}/cgql";
      };
      preStart = ''
        for x in {1..10}; do
          nc -z ${postgresqlIp} ${toString cfg.dbPort} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sudo -u ${cfg.dbAdminUser} -- psql ${cfg.db} < ${hasuraDbPerms}
      '';
      script = ''
        exec ${cfg.package}/bin/graphql-engine \
          serve \
          --server-port ${toString cfg.enginePort} \
          --enable-telemetry=false \
          --disable-cors
      '';
      serviceConfig = {
        # Hasura sometimes fails with sucessuful exit code, preventing monitoring detection. So we restart automatically.
        Restart = "always";
      };
    };
  };
}
