#!/bin/sh

# This script should be run after starting cardano-db-sync-extended, before starting the Hasura graphql-engine.
# cardano-db-sync-extended drops all views on startup, so migrations must be rolled back and re-applied to mirror the behaviour
# A Hasura CLI config.yml must be in the current directory

OPTS=$(getopt --long db-url:,\
  graphql-engine-path:,\
  hasura-cli-path:,\
  project-path:,\
  server-port:,\
  server-timeout:\
   -n 'parse-options' -- "$@")

if [ $? != 0 ] ; then echo "Failed parsing options." >&2 ; exit 1 ; fi

GRAPHQL_ENGINE=graphql-engine
HASURA_CLI=hasura
PROJECT_PATH=$PWD
SERVER_PORT=8090
TIMEOUT=30

while true; do
  case "$1" in
    --db-url              ) DATABASE_URL="$2"; shift 2;;
    --graphql-engine-path ) GRAPHQL_ENGINE="$2"; shift 2;;
    --hasura-cli-path     ) HASURA_CLI="$2"; shift 2;;
    --project-path        ) PROJECT_PATH="$2"; shift 2;;
    --server-port         ) SERVER_PORT="$2"; shift 2;;
    --server-timeout      ) TIMEOUT="$2"; shift 2;;
    -- ) shift; break ;;
    * ) break ;;
  esac
done

log() {
    TIMESTAMP=$(date -u "+%Y-%m-%dT%H:%M:%S.000+0000")
    MESSAGE=$1
    echo "{\"timestamp\":\"$TIMESTAMP\",\"level\":\"info\",\"type\":\"startup\",\"detail\":{\"kind\":\"migration-apply\",\"info\":\"$MESSAGE\"}}"
}

# wait for a port to be ready
wait_for_port() {
    local PORT=$1
    log "Waiting $TIMEOUT for $PORT to be ready"
    for i in $(seq 1 "$TIMEOUT");
    do
        nc -z localhost "$PORT" > /dev/null 2>&1 && log "port $PORT is ready" && return
        sleep 1
    done
    log "Failed waiting for $PORT" && exit 1
}

log "Starting graphql-engine to perform setup on port $SERVER_PORT. Inconsistent metadata is expected"

$GRAPHQL_ENGINE --database-url "$DATABASE_URL" \
  serve --enabled-apis="metadata" \
  --server-port=${SERVER_PORT} &
PID=$!

wait_for_port "$SERVER_PORT"

log "Applying migrations and metadata"
$HASURA_CLI --project "$PROJECT_PATH" migrate apply --down all
$HASURA_CLI --project "$PROJECT_PATH" migrate apply --up all
$HASURA_CLI --project "$PROJECT_PATH" metadata clear
$HASURA_CLI --project "$PROJECT_PATH" metadata apply

log "Killing server used to perform the setup"
kill $PID
