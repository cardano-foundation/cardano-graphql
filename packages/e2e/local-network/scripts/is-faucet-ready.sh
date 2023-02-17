#!/usr/bin/env bash

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"

cd "$root"

export PATH=$PWD/bin:$PATH

WALLET_ID="7991322ed68894d0f1fb645a74576c3780ab312c"
PORT=8090
FAUCET_WALLET='{"name": "Faucet", "mnemonic_sentence": ["fire", "method", "repair", "aware", "foot", "tray", "accuse", "brother", "popular", "olive", "find", "account", "sick", "rocket", "next"], "passphrase": "passphrase", "address_pool_gap": 20 }'

# Add Faucet wallet
curl -s http://localhost:"$PORT"/v2/wallets -H 'Content-Type: application/json' -H 'Accept: application/json' -d "$FAUCET_WALLET" > /dev/null

# Get wallet status
status=$(curl -s http://localhost:"$PORT"/v2/wallets/"$WALLET_ID" -H 'Content-Type: application/json' -H 'Accept: application/json' | jq .state.status)
epoch=$(curl -s http://localhost:"$PORT"/v2/wallets/"$WALLET_ID" -H 'Content-Type: application/json' -H 'Accept: application/json' | jq .tip.epoch_number)
totalBalance=$(curl -s http://localhost:"$PORT"/v2/wallets/"$WALLET_ID" -H 'Content-Type: application/json' -H 'Accept: application/json' | jq .balance.total.quantity)

if [[ "$status" == "\"ready\"" && $epoch -gt 4 && $totalBalance -gt 100000000 ]]; then # Faucet will be marked unhealthy if 100 or less tADA remains in the faucet wallet (initial balance is 13.5 billion tADA)
  exit 0
fi

exit 9

