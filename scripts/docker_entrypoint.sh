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
        allegra)
            export CURRENT_ERA_FIRST_SLOT=0
            export ERA_NAME=allegra
            ;;
        launchpad)
            export CURRENT_ERA_FIRST_SLOT=3531400
            export ERA_NAME=mary
            ;;
esac

exec node $SCRIPT_PATH