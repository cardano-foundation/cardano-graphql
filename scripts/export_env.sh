# Source this with the CONTEXT and NETWORK env set, containing the full path and
# supported network name.
# e.g.
# CONTEXT=$PWD NETWORK=mainnet . ./scripts/export_env.sh

export BIN_DIR=${CONTEXT}/bin
export CONFIG_DIR=${CONTEXT}/config/network/${NETWORK}
export SECRETS_DIR=${CONTEXT}/placeholder-secrets
export STATE_DIR=${CONTEXT}/state/network/${NETWORK}
case "$NETWORK" in
        mainnet)
            API_PORT=3100
            HASURA_PORT=8090
            METADATA_SERVER_URI="https://tokens.cardano.org"
            OGMIOS_PORT=1337
            PG_ADMIN_PORT=8442
            POSTGRES_PORT=5432
            ;;
        testnet)
            API_PORT=3101
            HASURA_PORT=8091
            METADATA_SERVER_URI="https://metadata.world.dev.cardano.org"
            OGMIOS_PORT=1338
            PG_ADMIN_PORT=8443
            POSTGRES_PORT=5443
            export CARDANO_NODE_VERSION=8.7.3
            ;;
        preprod)
            API_PORT=3102
            HASURA_PORT=8092
            METADATA_SERVER_URI="https://metadata.world.dev.cardano.org"
            OGMIOS_PORT=1339
            PG_ADMIN_PORT=8444
            POSTGRES_PORT=5444
            ;;
        preview)
            API_PORT=3103
            HASURA_PORT=8093
            METADATA_SERVER_URI="https://metadata.world.dev.cardano.org"
            OGMIOS_PORT=1340
            PG_ADMIN_PORT=8445
            POSTGRES_PORT=5445
            ;;
        vasil-dev)
            API_PORT=3104
            HASURA_PORT=8094
            METADATA_SERVER_URI="https://metadata.world.dev.cardano.org"
            OGMIOS_PORT=1341
            PG_ADMIN_PORT=8446
            POSTGRES_PORT=5446
            ;;
esac

export ALLOW_INTROSPECTION=true
export API_PORT
export CARDANO_NODE_CONFIG_PATH=${CONFIG_DIR}/cardano-node/config.json
export COMPOSE_DOCKER_CLI_BUILD=1
export DOCKER_BUILDKIT=1
export HASURA_CLI_PATH=${BIN_DIR}/hasura
export HASURA_CLI_EXT_PATH=${HASURA_CLI_PATH}
export HASURA_PORT
export HASURA_URI=http://localhost:${HASURA_PORT}
export METADATA_SERVER_URI
export NETWORK
export OGMIOS_PORT
export OGMIOS_HOST=localhost
export POSTGRES_DB_FILE=${SECRETS_DIR}/postgres_db
export POSTGRES_PASSWORD_FILE=${SECRETS_DIR}/postgres_password
export PG_ADMIN_PORT
export POSTGRES_PORT
export POSTGRES_USER_FILE=${SECRETS_DIR}/postgres_user
export POSTGRES_HOST=localhost
