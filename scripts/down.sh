#!/usr/bin/env bash

set -euo pipefail

export NETWORK=${NETWORK:-mainnet}

docker-compose -p $NETWORK down
