#!/usr/bin/env bash

set -euo pipefail

BIN_DIR=../bin

mkdir -p \
  state/network/mainnet/node-ipc \
  state/network/testnet/node-ipc \
  state/network/mary_qa/node-ipc \
  state/network/shelley_qa/node-ipc
cabal update
cd cardano-node
cabal install cardano-node \
  --install-method=copy \
  --installdir=${BIN_DIR} \
  --overwrite-policy=always \
  -f -systemd
cabal install cardano-cli \
  --install-method=copy \
  --installdir=${BIN_DIR} \
  --overwrite-policy=always \
  -f -systemd
curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | INSTALL_PATH=${BIN_DIR} bash
HASURA_GRAPHQL_ENABLE_TELEMETRY=false
${BIN_DIR}/hasura --skip-update-check update-cli --version v1.3.3
