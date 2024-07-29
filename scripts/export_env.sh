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
            METADATA_SERVER_URI="http://localhost:8080"
            OGMIOS_PORT=1337
            PG_ADMIN_PORT=8442
            POSTGRES_PORT=5432
            TOKEN_REGISTRY_PORT=8080
            CHAIN_FOLLOWER_START_SLOT=23068800
            CHAIN_FOLLOWER_START_ID="a650a3f398ba4a9427ec8c293e9f7156d81fd2f7ca849014d8d2c1156c359b3a"
            ;;
        testnet)
            API_PORT=3101
            HASURA_PORT=8091
            METADATA_SERVER_URI="https://metadata.world.dev.cardano.org"
            OGMIOS_PORT=1338
            PG_ADMIN_PORT=8443
            POSTGRES_PORT=5443
            TOKEN_REGISTRY_PORT=8081
            ;;
        preprod)
            API_PORT=3100
            HASURA_PORT=8090
            METADATA_SERVER_URI="http://localhost:8080"
            OGMIOS_PORT=1337
            PG_ADMIN_PORT=8442
            POSTGRES_PORT=5432
            TOKEN_REGISTRY_PORT=8080
            ;;
        preview)
            API_PORT=3103
            HASURA_PORT=8093
            METADATA_SERVER_URI="https://metadata.world.dev.cardano.org"
            OGMIOS_PORT=1340
            PG_ADMIN_PORT=8445
            POSTGRES_PORT=5445
            TOKEN_REGISTRY_PORT=8083
            ;;
        vasil-dev)
            API_PORT=3104
            HASURA_PORT=8094
            METADATA_SERVER_URI="https://metadata.world.dev.cardano.org"
            OGMIOS_PORT=1341
            PG_ADMIN_PORT=8446
            POSTGRES_PORT=5446
            TOKEN_REGISTRY_PORT=8084
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
export TOKEN_REGISTRY_TAG=latest
export CHAIN_FOLLOWER_START_SLOT
export CHAIN_FOLLOWER_START_ID
