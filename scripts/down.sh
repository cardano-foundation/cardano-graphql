#!/usr/bin/env bash

set -euo pipefail

docker-compose -p ${NETWORK:-mainnet} down
