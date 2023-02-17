#!/usr/bin/env bash

# This script locks funds behind a plutus validator script and then unlocks them.
# TODO: Remove this script once support for plutus scripts is added to the SDK.

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"

export PATH=$PWD/bin:$PATH
AMOUNT='100000000'
ALWAYS_SUCCEED_ADDR='addr_test1wpnlxv2xv9a9ucvnvzqakwepzl9ltx7jzgm53av2e9ncv4sysemm8'
SCRIPT_DATUM_VALUE=12
SCRIPT_DATUM_HASH='5e9d8bac576e8604e7c3526025bc146f5fa178173e3a5592d122687bd785b520'

clean() {
  rm pparams.json tx-script.build tx-script.signed test-alonzo.tx test-alonzo.signed balance.out fullUtxo.out
}

getAddressBalance() {
  cardano-cli query utxo \
      --address "$1" \
      --testnet-magic 888 > fullUtxo.out

  tail -n +3 fullUtxo.out | sort -k3 -nr > balance.out

  total_balance=0
  while read -r utxo; do
      utxo_balance=$(awk '{ print $3 }' <<< "${utxo}")
      total_balance=$(("$total_balance"+"$utxo_balance"))
  done < balance.out

  echo ${total_balance}
}

trap clean EXIT

while [ ! -S "$CARDANO_NODE_SOCKET_PATH" ]; do
  echo "plutus-transaction.sh: CARDANO_NODE_SOCKET_PATH: $CARDANO_NODE_SOCKET_PATH file doesn't exist, waiting..."
  sleep 2
done

genesisAddr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo2.vkey --testnet-magic 888)
utxo=$(cardano-cli query utxo --address "$genesisAddr" --testnet-magic 888 | awk 'NR == 3 {printf("%s#%s", $1, $2)}')
currentBalance=$(getAddressBalance "$ALWAYS_SUCCEED_ADDR")

# LOCK FUNDS

cardano-cli query protocol-parameters \
  --testnet-magic 888 \
  --out-file pparams.json

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --change-address "$genesisAddr" \
  --tx-in "$utxo" \
  --tx-out "$ALWAYS_SUCCEED_ADDR"+"$AMOUNT" \
  --tx-out-datum-hash "$SCRIPT_DATUM_HASH" \
  --protocol-params-file pparams.json \
  --out-file tx-script.build

cardano-cli transaction sign \
  --tx-body-file tx-script.build \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file tx-script.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file tx-script.signed

updatedBalance=$(getAddressBalance "$ALWAYS_SUCCEED_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$ALWAYS_SUCCEED_ADDR")
  sleep 1
done

# UNLOCK FUNDS

utxo=$(cardano-cli query utxo --address "$genesisAddr" --testnet-magic 888 | awk 'NR == 3 {printf("%s#%s", $1, $2)}')
scriptUtxo=$(cardano-cli query utxo --address "$ALWAYS_SUCCEED_ADDR" --testnet-magic 888 | awk 'NR == 3 {printf("%s#%s", $1, $2)}')
currentBalance=$(getAddressBalance "$ALWAYS_SUCCEED_ADDR")

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --tx-in "$scriptUtxo" \
  --tx-in-script-file scripts/contracts/alwayssucceeds.plutus \
  --tx-in-datum-value "$SCRIPT_DATUM_VALUE" \
  --tx-in-redeemer-value "$SCRIPT_DATUM_VALUE" \
  --tx-in-collateral "$utxo" \
  --change-address "$genesisAddr" \
  --protocol-params-file pparams.json \
  --out-file test-alonzo.tx

cardano-cli transaction sign \
  --tx-body-file test-alonzo.tx \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file test-alonzo.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file test-alonzo.signed

updatedBalance=$(getAddressBalance "$ALWAYS_SUCCEED_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$ALWAYS_SUCCEED_ADDR")
  sleep 1
done

echo "Done!"
