#!/bin/sh

set -e

SECRET_DIR="/run/secrets"
POSTGRES_DB=${POSTGRES_DB:-$(cat ${SECRET_DIR}/postgres_db)}
POSTGRES_USER=${POSTGRES_USER:-$(cat ${SECRET_DIR}/postgres_user)}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD:-$(cat ${SECRET_DIR}/postgres_password)}
HASURA_GRAPHQL_DATABASE_URL=postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@${POSTGRES_HOST}:${POSTGRES_PORT}/${POSTGRES_DB}

/bin/run-migrations.sh --db-url ${HASURA_GRAPHQL_DATABASE_URL}

# pass control to CMD
export HASURA_GRAPHQL_DATABASE_URL
echo "{\"timestamp\":\"$(date -u "+%Y-%m-%dT%H:%M:%S.000+0000")\",\"level\":\"info\",\"type\":\"startup\",\"detail\":{\"kind\":\"migration-apply\",\"info\":\"graphql-engine will now start in normal mode\"}}"
exec "$@"
