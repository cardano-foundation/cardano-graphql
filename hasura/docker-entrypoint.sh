#!/bin/sh

set -e

log() {
    TIMESTAMP=$(date -u "+%Y-%m-%dT%H:%M:%S.000+0000")
    MESSAGE=$1
    echo "{\"timestamp\":\"$TIMESTAMP\",\"level\":\"info\",\"type\":\"startup\",\"detail\":{\"kind\":\"migration-apply\",\"info\":\"$MESSAGE\"}}"
}

SECRET_DIR="/run/secrets"
POSTGRES_DB=${POSTGRES_DB:-$(cat ${SECRET_DIR}/postgres_db)}
POSTGRES_USER=${POSTGRES_USER:-$(cat ${SECRET_DIR}/postgres_user)}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD:-$(cat ${SECRET_DIR}/postgres_password)}
HASURA_GRAPHQL_DATABASE_URL=postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@${POSTGRES_HOST}:${POSTGRES_PORT}/${POSTGRES_DB}

# configure the target database for migrations
if [ ${HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR} ]; then
    log "database url for migrations is set by $HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR"
    HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL=$(printenv $HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR)
elif [ -z ${HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL+x} ]; then
    HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL=$HASURA_GRAPHQL_DATABASE_URL
fi
log "database url for migrations is set by HASURA_GRAPHQL_DATABASE_URL"

# Use 9691 port for running temporary instance.
# In case 9691 is occupied (according to docker networking), then this will fail.
# override with another port in that case
# TODO: Find a proper random port
if [ -z ${HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT+x} ]; then
    log "migrations server port env var is not set, defaulting to 9691"
    HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT=9691
fi

if [ -z ${HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT+x} ]; then
    log "server timeout is not set, defaulting to 30 seconds"
    HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT=30
fi

# wait for a port to be ready
wait_for_port() {
    local PORT=$1
    log "waiting $HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT for $PORT to be ready"
    for i in `seq 1 $HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT`;
    do
        nc -z localhost $PORT > /dev/null 2>&1 && log "port $PORT is ready" && return
        sleep 1
    done
    log "failed waiting for $PORT" && exit 1
}

log "starting graphql engine temporarily on port $HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT"
log "Metadata is expected to be inconsistent on initial startup"

# start graphql engine with metadata api enabled
graphql-engine --database-url "$HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL" \
               serve --enabled-apis="metadata" \
               --server-port=${HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT}  &
# store the pid to kill it later
PID=$!

# wait for port to be ready
wait_for_port $HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT

# Migrations & metadata
TEMP_PROJECT_DIR="/tmp/hasura-project"
log "applying migrations and metadata"
cd "$TEMP_PROJECT_DIR"
echo "version: 2" > config.yaml
echo "endpoint: http://localhost:$HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT" >> config.yaml
echo "metadata_directory: metadata" >> config.yaml
# cardano-db-sync drops all views on start, so we need to follow the same process for consistency
hasura-cli migrate apply --down all
hasura-cli migrate apply --up all
hasura-cli metadata clear
hasura-cli metadata apply

# kill graphql engine that we started earlier
log "killing temporary server"
kill $PID

# pass control to CMD
export HASURA_GRAPHQL_DATABASE_URL
log "graphql-engine will now start in normal mode"
exec "$@"