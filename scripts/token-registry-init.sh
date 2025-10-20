#!/bin/bash

export DB_USERNAME=$(cat /run/secrets/postgres_user)
export DB_PASSWORD=$(cat /run/secrets/postgres_password)
export DB_NAME=$(cat /run/secrets/postgres_db)
export DB_URL=jdbc:postgresql://${POSTGRES_HOST}:${POSTGRES_PORT}/${DB_NAME}?currentSchema=${DB_SCHEMA}
export GITHUB_TMP_FOLDER='/tmp'
export GITHUB_FORCE_CLONE='false'
export CIP_QUERY_PRIORITY='CIP_68,CIP_26'
export STORE_BLOCKS_EPOCH_CALCULATION_INTERVAL='14400'
export STORE_SYNCAUTOSTART=true

case "$NETWORK" in
    mainnet)
      export GITHUB_ORGANIZATION=cardano-foundation
      export GITHUB_PROJECT_NAME=cardano-token-registry
      export GITHUB_MAPPINGS_FOLDER=mappings
      # Yaci Store / Cardano Node Configuration  
      export STORE_CARDANO_PROTOCOL_MAGIC='764824073'
      export STORE_CARDANO_HOST='backbone.mainnet.cardanofoundation.org'
      export STORE_CARDANO_PORT='3001'
      export STORE_CARDANO_SYNC_START_SLOT='65836843'
      export STORE_CARDANO_SYNC_START_BLOCKHASH='cb09cae9c54026afebfe6124189600fc0f76c2299bc9f9c32305944979a12fed'
      ;;
    preprod|preview|testnet)
      export GITHUB_ORGANIZATION=input-output-hk
      export GITHUB_PROJECT_NAME=metadata-registry-testnet
      export GITHUB_MAPPINGS_FOLDER=registry
      # Yaci Store / Cardano Node Configuration - Preprod Network
      export STORE_CARDANO_PROTOCOL_MAGIC='1'
      export STORE_CARDANO_HOST='preprod-node.play.dev.cardano.org'
      export STORE_CARDANO_PORT='3001'
      # we don't know exactly when CIP-68 was introduced so we start from genesis for now on
      export STORE_CARDANO_SYNC_START_SLOT=0
      export STORE_CARDANO_SYNC_START_BLOCKHASH=
      ;;
esac
echo Using Github Repository $GITHUB_ORGANIZATION/$GITHUB_PROJECT_NAME as token registry
java -jar /app/app.jar