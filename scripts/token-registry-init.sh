#!/bin/bash

export DB_USERNAME=$(cat /run/secrets/postgres_user)
export DB_PASSWORD=$(cat /run/secrets/postgres_password)
export DB_NAME=$(cat /run/secrets/postgres_db)
export DB_URL=jdbc:postgresql://${POSTGRES_HOST}:${POSTGRES_PORT}/${DB_NAME}?currentSchema=${DB_SCHEMA}

case "$NETWORK" in
    mainnet)
      export GITHUB_ORGANIZATION=cardano-foundation
      export GITHUB_PROJECT_NAME=cardano-token-registry
      export GITHUB_MAPPINGS_FOLDER=mappings
      ;;
    preprod|preview|testnet)
      export GITHUB_ORGANIZATION=input-output-hk
      export GITHUB_PROJECT_NAME=metadata-registry-testnet
      export GITHUB_MAPPINGS_FOLDER=registry
      ;;
esac
echo Using Github Repository $GITHUB_ORGANIZATION/$GITHUB_PROJECT_NAME as token registry
java -jar /app/app.jar