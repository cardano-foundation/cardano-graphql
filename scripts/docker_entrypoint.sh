#!/usr/bin/env bash

set -euo pipefail
SCRIPT_PATH=$1

case "$NETWORK" in
        mainnet)
            export CURRENT_ERA_FIRST_SLOT=16588800
            export ERA_NAME=allegra
            ;;
        testnet)
            export CURRENT_ERA_FIRST_SLOT=13694400
            export ERA_NAME=allegra
            ;;
esac

exec node $SCRIPT_PATH