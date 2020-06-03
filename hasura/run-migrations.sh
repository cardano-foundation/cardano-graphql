#!/bin/sh
OPTS=$(getopt --long db-url:,server-port:,server-timeout: -n 'parse-options' -- "$@")
if [ $? != 0 ] ; then echo "Failed parsing options." >&2 ; exit 1 ; fi

SERVER_PORT=9691
TIMEOUT=30

while true; do
  case "$1" in
    --db-url         ) DATABASE_URL="$2"; shift 2;;
    --server-port    ) SERVER_PORT="$2"; shift 2;;
    --server-timeout ) TIMEOUT="$2"; shift 2;;
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
    log "waiting $TIMEOUT for $PORT to be ready"
    for i in $(seq 1 "$TIMEOUT");
    do
        nc -z localhost "$PORT" > /dev/null 2>&1 && log "port $PORT is ready" && return
        sleep 1
    done
    log "failed waiting for $PORT" && exit 1
}

log "starting graphql engine temporarily on port $SERVER_PORT"
log "Metadata is expected to be inconsistent on initial startup"

graphql-engine --database-url "$DATABASE_URL" \
               serve --enabled-apis="metadata" \
               --server-port="${SERVER_PORT}"  &
PID=$!

wait_for_port "$SERVER_PORT"

# Migrations & metadata
TEMP_PROJECT_DIR="/tmp/hasura-project"
log "applying migrations and metadata"
cd "$TEMP_PROJECT_DIR" || exit
echo "version: 2" > config.yaml
echo "endpoint: http://localhost:$SERVER_PORT" >> config.yaml
echo "metadata_directory: metadata" >> config.yaml
# cardano-db-sync drops all views on start, so we need to follow the same process for consistency
hasura migrate apply --down all
hasura migrate apply --up all
hasura metadata clear
hasura metadata apply

log "killing temporary server"
kill $PID
