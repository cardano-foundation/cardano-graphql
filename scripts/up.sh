#!/usr/bin/env bash

set -eo pipefail

export NETWORK=${NETWORK:-mainnet}
export API_PORT=${API_PORT:-3100}
export OGMIOS_PORT=${OGMIOS_PORT:-1337}
export POSTGRES_PORT=${POSTGRES_PORT:-5433}

while [[ "$#" -gt 0 ]]
do
  case $1 in
    --use-cache)
      # Docker Buildkit
      export DOCKER_BUILDKIT=1
      export COMPOSE_DOCKER_CLI_BUILD=1
      ;;
  esac
  shift
done

docker-compose -p $NETWORK up -d --build && \
docker-compose -p $NETWORK logs -f
