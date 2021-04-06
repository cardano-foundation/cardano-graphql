#!/bin/sh

set -e

SECRET_DIR=${SECRET_DIR:-/run/secrets}
POSTGRES_DB=${POSTGRES_DB:-$(cat ${SECRET_DIR}/postgres_db)}
POSTGRES_USER=${POSTGRES_USER:-$(cat ${SECRET_DIR}/postgres_user)}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD:-$(cat ${SECRET_DIR}/postgres_password)}
HASURA_GRAPHQL_DATABASE_URL=postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@${POSTGRES_HOST}:${POSTGRES_PORT}/${POSTGRES_DB}

exec graphql-engine \
  --disable-cors \
  --database-url $HASURA_GRAPHQL_DATABASE_URL \
  serve
