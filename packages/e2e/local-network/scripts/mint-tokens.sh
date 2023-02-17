#!/usr/bin/env bash

# This script mint native tokens to genesis address
set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"

export PATH=$PWD/bin:$PATH
TOKENS=(744d494e 74425443 74455448)
AMOUNT='13500000000000000'

clean() {
  rm -rf tx.raw tx.signed
  rm -rf shelley
}

trap clean EXIT

while [ ! -S "$CARDANO_NODE_SOCKET_PATH" ]; do
  echo "mint-tokens.sh: CARDANO_NODE_SOCKET_PATH: $CARDANO_NODE_SOCKET_PATH file doesn't exist, waiting..."
  sleep 2
done

echo "Create Mary-era minting policy"
cat >network-files/utxo-keys/minting-policy.json <<EOL
{
  "keyHash": "$(cardano-cli address key-hash --payment-verification-key-file network-files/utxo-keys/utxo1.vkey)",
  "type": "sig"
}
EOL

currencySymbol=$(cardano-cli transaction policyid --script-file network-files/utxo-keys/minting-policy.json)
addr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo1.vkey --testnet-magic 888)
faucetAddr="addr_test1qqen0wpmhg7fhkus45lyv4wju26cecgu6avplrnm6dgvuk6qel5hu3u3q0fht53ly97yx95hkt56j37ch07pesf6s4pqh5gd4e"

# Spend the first UTxO
utxo=$(cardano-cli query utxo --address "$addr" --testnet-magic 888 | awk 'NR == 3 {printf("%s#%s", $1, $2)}')

# Build token list. We start with a token with empty asset name as this is often an edge case for devs.
tokenList="${AMOUNT} ${currencySymbol}"
for i in "${!TOKENS[@]}"; do
  tokenList="${tokenList}+${AMOUNT} ${currencySymbol}.${TOKENS[i]}"
done


cardano-cli transaction build \
  --babbage-era \
  --change-address "$faucetAddr" \
  --tx-in "$utxo" \
  --tx-out "$faucetAddr"+10000000+"$tokenList" \
  --mint "$tokenList" \
  --mint-script-file network-files/utxo-keys/minting-policy.json \
  --testnet-magic 888 \
  --out-file tx.raw

cardano-cli transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file network-files/utxo-keys/utxo1.skey \
  --testnet-magic 888 \
  --out-file tx.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file tx.signed
