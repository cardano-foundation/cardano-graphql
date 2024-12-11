# Source this with the CONTEXT and NETWORK env set, containing the full path and
# supported network name.
# e.g.
# CONTEXT=$PWD NETWORK=mainnet . ./scripts/export_env.sh

export BIN_DIR=${CONTEXT}/bin
export CONFIG_DIR=${CONTEXT}/config/network/${NETWORK}
export SECRETS_DIR=${CONTEXT}/placeholder-secrets
export STATE_DIR=${CONTEXT}/state/network/${NETWORK}

export API_PORT=3100
export HASURA_PORT=8091
export METADATA_SERVER_URI="http://localhost:8080"
export OGMIOS_PORT=1337
export PG_ADMIN_PORT=8444
export POSTGRES_PORT=5433
export TOKEN_REGISTRY_PORT=8080


export ALLOW_INTROSPECTION=true
export CARDANO_NODE_CONFIG_PATH=${CONFIG_DIR}/cardano-node/config.json
export COMPOSE_DOCKER_CLI_BUILD=1
export DOCKER_BUILDKIT=1
export HASURA_CLI_PATH=/usr/local/bin/hasura
export HASURA_CLI_EXT_PATH=${HASURA_CLI_PATH}
export HASURA_URI=http://localhost:${HASURA_PORT}
export OGMIOS_HOST=localhost
export POSTGRES_DB=cexplorer
export POSTGRES_PASSWORD=doNoUseThisSecret!
export POSTGRES_USER=postgres
export POSTGRES_HOST=localhost
export TOKEN_REGISTRY_TAG=latest
echo "exported dev env vars for ${NETWORK}"
