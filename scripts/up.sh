#!/usr/bin/env bash

set -eo pipefail

export NETWORK=${NETWORK:-mainnet}
export API_PORT=${API_PORT:-3100}
export OGMIOS_PORT=${OGMIOS_PORT:-1337}
export POSTGRES_PORT=${POSTGRES_PORT:-5433}

if [[ $1 ]]; then
  # Docker Buildkit
  export DOCKER_BUILDKIT=1
  export COMPOSE_DOCKER_CLI_BUILD=1
fi

docker-compose -p $NETWORK up -d --build && \
docker-compose -p $NETWORK logs -f
