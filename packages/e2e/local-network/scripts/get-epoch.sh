#!/usr/bin/env bash

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"

cd "$root"

export PATH=$PWD/bin:$PATH
export CARDANO_NODE_SOCKET_PATH=$PWD/network-files/node-sp1/node.sock

if [ ! -S "$CARDANO_NODE_SOCKET_PATH" ]; then
  echo "-1"
else
  cardano-cli query tip --testnet-magic 888 | jq .epoch
fi

