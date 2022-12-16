
{ lib, pkgs, config, ... }:
let
  cfg = config.services.cardano-graphql;
  selfPkgs = import ../pkgs.nix {};
in {
  options = {
    services.cardano-graphql = {
      enable = lib.mkEnableOption "cardano-explorer graphql service";

      enginePort = lib.mkOption {
        type = lib.types.int;
        default = 9999;
      };

      listenAddress = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      loggerMinSeverity = lib.mkOption {
        type = lib.types.str;
        default = "info";
      };

      port = lib.mkOption {
        type = lib.types.int;
        default = 3100;
      };

      cardanoNodeConfigPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };

      hasuraIp = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };

      hasuraProtocol = lib.mkOption {
        type = lib.types.str;
        default = "http";
      };

      ogmiosHost = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };

      ogmiosPort = lib.mkOption {
        type = lib.types.int;
        default = 1337;
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
      allowIntrospection = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Allows introspection queries";
      };
      allowListPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Source directory or file to generate allow-list from";
      };
      pollingIntervalAdaSupply = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
      };
      maxQueryComplexity = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
      };
    };
  };
  config = let
    # TODO: there has to be a better way to handle boolean env vars in nodejs???
    boolToNodeJSEnv = bool: if bool then "true" else "false";
    frontend = selfPkgs.packages.cardano-graphql;
    persistgraphql = selfPkgs.packages.persistgraphql;
    hasuraBaseUri = "${cfg.hasuraProtocol}://${cfg.hasuraIp}:${toString cfg.enginePort}";
    pluginLibPath = pkgs.lib.makeLibraryPath [
      pkgs.stdenv.cc.cc.lib
    ];
  in lib.mkIf cfg.enable {
    systemd.services.cardano-graphql = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "graphql-engine.service" ];
      after = [ "graphql-engine.service" ];
      environment = lib.filterAttrs (k: v: v != null) {
        CARDANO_NODE_CONFIG_PATH = cfg.cardanoNodeConfigPath;
        HASURA_GRAPHQL_ENABLE_TELEMETRY = toString false;
        HASURA_URI = hasuraBaseUri;
        LOGGER_MIN_SEVERITY = cfg.loggerMinSeverity;
        OGMIOS_HOST = cfg.ogmiosHost;
        OGMIOS_PORT = toString cfg.ogmiosPort;
        PROMETHEUS_METRICS = boolToNodeJSEnv cfg.enablePrometheus;
        TRACING = boolToNodeJSEnv (cfg.enableTracing || cfg.enablePrometheus);
        ALLOW_INTROSPECTION = boolToNodeJSEnv cfg.allowIntrospection;
        CACHE_ENABLED = boolToNodeJSEnv cfg.enableCache;
        API_PORT = toString cfg.port;
      } //
      (lib.optionalAttrs (cfg.allowedOrigins != null) { ALLOWED_ORIGINS = cfg.allowedOrigins; }) //
      (lib.optionalAttrs (cfg.listenAddress != null) { LISTEN_ADDRESS = cfg.listenAddress; }) //
      (lib.optionalAttrs (cfg.pollingIntervalAdaSupply != null) { POLLING_INTERVAL_ADA_SUPPLY = toString cfg.pollingIntervalAdaSupply; }) //
      (lib.optionalAttrs (cfg.queryDepthLimit != null) { QUERY_DEPTH_LIMIT = toString cfg.queryDepthLimit; }) //
      (lib.optionalAttrs (cfg.allowListPath != null) { ALLOW_LIST_PATH = cfg.allowListPath; }) //
      (lib.optionalAttrs (cfg.maxQueryComplexity != null) { MAX_QUERY_COMPLEXITY = toString cfg.maxQueryComplexity; });
      path = with pkgs; [ netcat curl frontend glibc.bin patchelf ];
      script = ''
        exec cardano-graphql
      '';
    };
  };
}
