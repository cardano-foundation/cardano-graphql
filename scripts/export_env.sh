# Source this with the CONTEXT and NETWORK env set, containing the full path and 
# supported network name.
# e.g.
# CONTEXT=$PWD NETWORK=mainnet . ./scripts/export_env.sh

export BIN_DIR=${CONTEXT}/bin
export CONFIG_DIR=${CONTEXT}/config/network/${NETWORK}
export SECRETS_DIR=${CONTEXT}/config/secrets
export STATE_DIR=${CONTEXT}/state/network/${NETWORK}

case "$NETWORK" in
        mainnet)
            API_PORT=3100
            CURRENT_ERA_FIRST_SLOT=16588800
            ERA_NAME=allegra
            HASURA_PORT=8090
            POSTGRES_PORT=5442
            ;;
        testnet)
            API_PORT=3101
            CURRENT_ERA_FIRST_SLOT=13694400
            ERA_NAME=allegra
            HASURA_PORT=8091
            POSTGRES_PORT=5443
            ;;
esac

export ALLOW_INTROSPECTION=true
export API_PORT
export CARDANO_CLI_PATH=${BIN_DIR}/cardano-cli
export CARDANO_NODE_SOCKET_PATH=${STATE_DIR}/node-ipc/node.socket
export CURRENT_ERA_FIRST_SLOT
export ERA_NAME
export GENESIS_FILE_BYRON=${CONFIG_DIR}/genesis/byron.json
export GENESIS_FILE_SHELLEY=${CONFIG_DIR}/genesis/shelley.json
export HASURA_CLI_PATH=${BIN_DIR}/hasura
export HASURA_PORT
export HASURA_URI=http://localhost:${HASURA_PORT}
export NETWORK
export POSTGRES_DB_FILE=${SECRETS_DIR}/postgres_db
export POSTGRES_PASSWORD_FILE=${SECRETS_DIR}/postgres_password
export POSTGRES_PORT
export POSTGRES_USER_FILE=${SECRETS_DIR}/postgres_user
export POSTGRES_HOST=localhost
