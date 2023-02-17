#!/usr/bin/env bash

set -euo pipefail

ROOT=network-files

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"

export PATH=$PWD/bin:$PATH

source ./scripts/nodes-configuration.sh

echo "Clean old state and logs"
./scripts/clean.sh

# Kill all child processes on Ctrl+C
trap 'kill 0' INT

echo "Run"
./scripts/make-babbage.sh
./network-files/run/all.sh &

for ID in ${SP_NODES_ID}; do
  if [ -f "./scripts/pools/update-node-sp$ID.sh" ]; # Only update the pool if a script exists for that pool.
  then
    CARDANO_NODE_SOCKET_PATH=$PWD/network-files/node-sp"$ID"/node.sock ./scripts/pools/update-node-sp"$ID".sh
  fi
done

CARDANO_NODE_SOCKET_PATH=$PWD/network-files/node-sp1/node.sock ./scripts/plutus-transaction.sh
CARDANO_NODE_SOCKET_PATH=$PWD/network-files/node-sp1/node.sock ./scripts/reference-input-transaction.sh
CARDANO_NODE_SOCKET_PATH=$PWD/network-files/node-sp1/node.sock ./scripts/mint-tokens.sh
CARDANO_NODE_SOCKET_PATH=$PWD/network-files/node-sp1/node.sock ./scripts/setup-wallets.sh

wait
