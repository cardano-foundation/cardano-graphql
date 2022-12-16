#!/usr/bin/env bash

set -euo pipefail

BIN_DIR=./bin
curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | INSTALL_PATH=${BIN_DIR} bash
HASURA_GRAPHQL_ENABLE_TELEMETRY=false
${BIN_DIR}/hasura --skip-update-check update-cli --version v2.11.1
