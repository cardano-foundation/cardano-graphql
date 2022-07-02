
{ lib, pkgs, config, ... }:
let
  cfg = config.services.cardano-graphql;
  selfPkgs = import ../pkgs.nix {};
in {
  options = {
    services.cardano-graphql = {
      enable = lib.mkEnableOption "cardano-explorer graphql service";

      dbHost = lib.mkOption {
        type = lib.types.str;
        default = "/run/postgresql";
      };

      dbPassword = lib.mkOption {
        type = lib.types.str;
        default = ''""'';
      };

      dbPort = lib.mkOption {
        type = lib.types.int;
        default = 5432;
      };

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
      metadataServerUri = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
      pollingIntervalAdaSupply = lib.mkOption {
        type = lib.types.nullOr lib.types.int;
        default = null;
      };
      assetMetadataUpdateInterval = lib.mkOption {
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
    hasura-cli = selfPkgs.packages.hasura-cli;
    hasura-cli-ext = selfPkgs.packages.hasura-cli-ext;
    hasuraBaseUri = "${cfg.hasuraProtocol}://${cfg.hasuraIp}:${toString cfg.enginePort}";
    pluginLibPath = pkgs.lib.makeLibraryPath [
      pkgs.stdenv.cc.cc.lib
    ];
    installHasuraCLI = ''
      # always start with no plugins so future upgrades will work
      rm -rf ~/.hasura/plugins
      mkdir -p ~/.hasura/plugins/store/cli-ext/v${hasura-cli-ext.version}
      ln -s ${hasura-cli-ext}/bin/cli-ext-hasura-linux ~/.hasura/plugins/store/cli-ext/v${hasura-cli-ext.version}/cli-ext-hasura-linux
    '';
  in lib.mkIf cfg.enable {
    systemd.services.cardano-graphql = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "graphql-engine.service" ];
      after = [ "graphql-engine.service" ];
      environment = lib.filterAttrs (k: v: v != null) {
        CARDANO_NODE_CONFIG_PATH = cfg.cardanoNodeConfigPath;
        HASURA_CLI_PATH = hasura-cli + "/bin/hasura";
        HASURA_GRAPHQL_ENABLE_TELEMETRY = toString false;
        HASURA_URI = hasuraBaseUri;
        LOGGER_MIN_SEVERITY = cfg.loggerMinSeverity;
        OGMIOS_HOST = cfg.ogmiosHost;
        OGMIOS_PORT = toString cfg.ogmiosPort;
        POSTGRES_DB = cfg.db;
        POSTGRES_HOST = cfg.dbHost;
        POSTGRES_PASSWORD = cfg.dbPassword;
        POSTGRES_PORT = toString cfg.dbPort;
        POSTGRES_USER = cfg.dbUser;
        PROMETHEUS_METRICS = boolToNodeJSEnv cfg.enablePrometheus;
        TRACING = boolToNodeJSEnv (cfg.enableTracing || cfg.enablePrometheus);
        ALLOW_INTROSPECTION = boolToNodeJSEnv cfg.allowIntrospection;
        CACHE_ENABLED = boolToNodeJSEnv cfg.enableCache;
        API_PORT = toString cfg.port;
      } //
      (lib.optionalAttrs (cfg.allowedOrigins != null) { ALLOWED_ORIGINS = cfg.allowedOrigins; }) //
      (lib.optionalAttrs (cfg.listenAddress != null) { LISTEN_ADDRESS = cfg.listenAddress; }) //
      (lib.optionalAttrs (cfg.metadataServerUri != null) { METADATA_SERVER_URI = toString cfg.metadataServerUri; }) //
      (lib.optionalAttrs (cfg.pollingIntervalAdaSupply != null) { POLLING_INTERVAL_ADA_SUPPLY = toString cfg.pollingIntervalAdaSupply; }) //
      (lib.optionalAttrs (cfg.assetMetadataUpdateInterval != null) { ASSET_METADATA_UPDATE_INTERVAL = toString cfg.assetMetadataUpdateInterval; }) //
      (lib.optionalAttrs (cfg.queryDepthLimit != null) { QUERY_DEPTH_LIMIT = toString cfg.queryDepthLimit; }) //
      (lib.optionalAttrs (cfg.allowListPath != null) { ALLOW_LIST_PATH = cfg.allowListPath; }) //
      (lib.optionalAttrs (cfg.maxQueryComplexity != null) { MAX_QUERY_COMPLEXITY = toString cfg.maxQueryComplexity; });
      path = with pkgs; [ netcat curl postgresql frontend hasura-cli glibc.bin patchelf ];
      preStart = ''
        set -exuo pipefail
        ${installHasuraCLI}
      '';
      script = ''
        exec cardano-graphql
      '';
    };
  };
}
