#!/usr/bin/env bash

PROJECT_NAME=${1:-cardano-graphql}

docker ps \
  -f name="$PROJECT_NAME_postgres" \
  -f name="$PROJECT_NAME_cardano-node" \
  -f name="$PROJECT_NAME_cardano-db-sync-extended" \
  -f name="$PROJECT_NAME_hasura" \
  -f name="$PROJECT_NAME_cardano-graphql" \
  --format '{{.Names}}' | tee >(wc -l)