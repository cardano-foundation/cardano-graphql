# WARNING!!! THIS IS BROKEN! DO NOT USE!

{ lib, pkgs, config, ... }:
let
  cfg = config.services.cardano-graphql;
  sources = import ../sources.nix;
in {
  options = {
    services.cardano-graphql = {
      enable = lib.mkEnableOption "cardano-explorer graphql service";

      dbUser = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
      };

      db = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
      };

      enginePort = lib.mkOption {
        type = lib.types.int;
        default = 9999;
      };

      port = lib.mkOption {
        type = lib.types.int;
        default = 3100;
      };

      hasuraIp = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };

      hasuraProtocol = lib.mkOption {
        type = lib.types.str;
        default = "http";
      };

      enablePrometheus = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };

      enableTracing = lib.mkEnableOption "tracing";
      enableCache = lib.mkEnableOption "cache";

      queryDepthLimit = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
      };

      allowedOrigins = lib.mkOption {
        type = lib.types.nullOr (lib.types.separatedString " ");
        default = null;
      };
      filterHasuraOperations = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Filters hasura operations allowed to minimal set needed";
      };
    };
  };
  config = let
    # TODO: there has to be a better way to handle boolean env vars in nodejs???
    boolToNodeJSEnv = bool: if bool then "true" else "false";
    frontend = (import ../../.).cardano-graphql;
    hasuraBaseUri = cfg.hasuraProtocol + "://" + cfg.hasuraIp + ":" + (toString cfg.enginePort);
    hasuraDbMetadata = ../../hasura/migrations/metadata.json;
  in lib.mkIf cfg.enable {
    systemd.services.cardano-graphql = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "graphql-engine.service" ];
      environment = {
        HASURA_URI = hasuraBaseUri;
        PROMETHEUS_METRICS = boolToNodeJSEnv cfg.enablePrometheus;
        TRACING = boolToNodeJSEnv (cfg.enableTracing || cfg.enablePrometheus);
        CACHE_ENABLED = boolToNodeJSEnv cfg.enableCache;
        API_PORT = toString cfg.port;
      } //
      (lib.optionalAttrs (cfg.allowedOrigins != null) { ALLOWED_ORIGINS = cfg.allowedOrigins; }) //
      (lib.optionalAttrs (cfg.queryDepthLimit != null) { QUERY_DEPTH_LIMIT = toString cfg.queryDepthLimit; });
      path = with pkgs; [ netcat curl postgresql nodejs-12_x ];
      preStart = ''
        set -euo pipefail
        for x in {1..10}; do
          nc -z ${cfg.hasuraIp} ${toString cfg.enginePort} && break
          echo loop $x: waiting for graphql-engine 2 sec...
          sleep 2
        done
        curl -d'{"type":"replace_metadata", "args":'$(${pkgs.jq}/bin/jq -c < ${hasuraDbMetadata})'}' ${hasuraBaseUri}/v1/query
        ${lib.optionalString cfg.filterHasuraOperations ''
          echo "setting filter for allowed hasura operations"
          ${frontend}/bin/hasura-allow-operations-in "${frontend}/src/example_queries/**/*.graphql"
          
        ''}
      '';
      script = ''
        exec ${frontend}/bin/cardano-graphql

      '';
    };
  };
}
