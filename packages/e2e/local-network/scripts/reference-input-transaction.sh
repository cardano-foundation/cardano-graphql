#!/usr/bin/env bash

# This script uses all new Babbage fields for Txs and UTXOs
# TODO: Remove this script once support for plutus scripts is added to the SDK.

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"

export PATH=$PWD/bin:$PATH
AMOUNT='100000000'
REFERENCE_INPUT_SCRIPT_ADDR='addr_test1wzem0yuxjqyrmzvrsr8xfqhumyy555ngyjxw7wrg2pav90q8cagu2'
REFERENCE_INPUT_ADDR='addr_test1wqnp362vmvr8jtc946d3a3utqgclfdl5y9d3kn849e359hst7hkqk'
REFERENCE_SCRIPT_ADDR='addr_test1wz3937ykmlcaqxkf4z7stxpsfwfn4re7ncy48yu8vutcpxgnj28k0'

clean() {
  rm pparams.json tx-script.build tx-script.signed test-babbage.tx test-babbage.signed test-babbage2.tx test-babbage2.signed balance.out fullUtxo.out
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

getBiggestUtxo() {
  cardano-cli query utxo \
      --address "$1" \
      --testnet-magic 888 > fullUtxo.out

  tail -n +3 fullUtxo.out | sort -k3 -nr > balance.out

  current_biggest=0
  utxo_id=''
  while read -r utxo; do
    utxo_balance=$(awk '{ print $3 }' <<< "${utxo}")

    if [[ "$utxo_balance" -ge "$current_biggest" ]];
    then
      current_biggest=$utxo_balance
      utxo_id=$(awk '{printf("%s#%s", $1, $2)}' <<< "${utxo}")
    fi
  done < balance.out

  echo "${utxo_id} ${current_biggest}"
}

trap clean EXIT

while [ ! -S "$CARDANO_NODE_SOCKET_PATH" ]; do
  echo "plutus-transaction.sh: CARDANO_NODE_SOCKET_PATH: $CARDANO_NODE_SOCKET_PATH file doesn't exist, waiting..."
  sleep 2
done

# LOCK REFERENCE UTXO

genesisAddr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo2.vkey --testnet-magic 888)
utxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$genesisAddr")")
currentBalance=$(getAddressBalance "$REFERENCE_INPUT_ADDR")

echo "Locking reference UTXO..."
cardano-cli query protocol-parameters \
  --testnet-magic 888 \
  --out-file pparams.json

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --change-address "$genesisAddr" \
  --tx-in "$utxo" \
  --tx-out "$REFERENCE_INPUT_ADDR"+"$AMOUNT" \
  --tx-out-inline-datum-value 42 \
  --protocol-params-file pparams.json \
  --out-file tx-script.build

cardano-cli transaction sign \
  --tx-body-file tx-script.build \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file tx-script.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file tx-script.signed

updatedBalance=$(getAddressBalance "$REFERENCE_INPUT_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$REFERENCE_INPUT_ADDR")
  sleep 1
done
echo "Locked"

# LOCK REFERENCE SCRIPT UTXO

genesisAddr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo2.vkey --testnet-magic 888)
utxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$genesisAddr")")
currentBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

echo "Locking reference script UTXO (Multisignature)..."

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --change-address "$genesisAddr" \
  --tx-in "$utxo" \
  --tx-out "$REFERENCE_SCRIPT_ADDR"+"$AMOUNT" \
  --tx-out-reference-script-file scripts/contracts/multisignature.json \
  --protocol-params-file pparams.json \
  --out-file tx-script.build

cardano-cli transaction sign \
  --tx-body-file tx-script.build \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file tx-script.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file tx-script.signed

updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")
  sleep 1
done
echo "Locked"

genesisAddr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo2.vkey --testnet-magic 888)
utxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$genesisAddr")")
currentBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

echo "Locking reference script UTXO (Timelock)..."

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --change-address "$genesisAddr" \
  --tx-in "$utxo" \
  --tx-out "$REFERENCE_SCRIPT_ADDR"+"$AMOUNT" \
  --tx-out-reference-script-file scripts/contracts/timelock.json \
  --protocol-params-file pparams.json \
  --out-file tx-script.build

cardano-cli transaction sign \
  --tx-body-file tx-script.build \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file tx-script.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file tx-script.signed

updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")
  sleep 1
done
echo "Locked"

genesisAddr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo2.vkey --testnet-magic 888)
utxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$genesisAddr")")
currentBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

echo "Locking reference script UTXO (Plutus V1)..."

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --change-address "$genesisAddr" \
  --tx-in "$utxo" \
  --tx-out "$REFERENCE_SCRIPT_ADDR"+"$AMOUNT" \
  --tx-out-reference-script-file scripts/contracts/alwayssucceeds.plutus \
  --protocol-params-file pparams.json \
  --out-file tx-script.build

cardano-cli transaction sign \
  --tx-body-file tx-script.build \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file tx-script.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file tx-script.signed

updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")
  sleep 1
done
echo "Locked"

genesisAddr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo2.vkey --testnet-magic 888)
utxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$genesisAddr")")
currentBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

echo "Locking reference script UTXO (Plutus V2)..."

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --change-address "$genesisAddr" \
  --tx-in "$utxo" \
  --tx-out "$REFERENCE_SCRIPT_ADDR"+"$AMOUNT" \
  --tx-out-reference-script-file scripts/contracts/reference-input.plutus \
  --protocol-params-file pparams.json \
  --out-file tx-script.build

cardano-cli transaction sign \
  --tx-body-file tx-script.build \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file tx-script.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file tx-script.signed

updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$REFERENCE_SCRIPT_ADDR")
  sleep 1
done
echo "Locked"

# LOCK FUNDS IN SCRIPT
echo "Locking funds in script..."
utxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$genesisAddr")")
currentBalance=$(getAddressBalance "$REFERENCE_INPUT_SCRIPT_ADDR")

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --tx-in "$utxo" \
  --tx-out "$REFERENCE_INPUT_SCRIPT_ADDR"+"$AMOUNT" \
  --tx-out-inline-datum-file scripts/contracts/unit.json \
  --change-address "$genesisAddr" \
  --protocol-params-file pparams.json \
  --out-file test-babbage.tx

cardano-cli transaction sign \
  --tx-body-file test-babbage.tx \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file test-babbage.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file test-babbage.signed

updatedBalance=$(getAddressBalance "$REFERENCE_INPUT_SCRIPT_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$REFERENCE_INPUT_SCRIPT_ADDR")
  sleep 1
done
echo "Locked"

# UNLOCK FUNDS
echo "Unlocking funds from script..."
utxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$genesisAddr")")
utxoVal=$(awk '{printf("%s", $2)}' <<< "$(getBiggestUtxo "$genesisAddr")")
referenceInputUtxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$REFERENCE_INPUT_ADDR")")
scriptUtxo=$(awk '{printf("%s", $1)}' <<< "$(getBiggestUtxo "$REFERENCE_INPUT_SCRIPT_ADDR")")
currentBalance=$(getAddressBalance "$REFERENCE_INPUT_SCRIPT_ADDR")
returnCollateralVal=$(("$utxoVal" - 1450000))

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 888 \
  --tx-in-collateral "$utxo" \
  --tx-in "$utxo" \
  --tx-in "$scriptUtxo" \
  --tx-in-script-file scripts/contracts/reference-input.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-value 42 \
  --read-only-tx-in-reference "$referenceInputUtxo" \
  --change-address "$genesisAddr" \
  --protocol-params-file pparams.json \
  --tx-out-return-collateral "$REFERENCE_INPUT_ADDR"+"$returnCollateralVal" \
  --tx-total-collateral 1450000 \
  --out-file test-babbage2.tx

cardano-cli transaction sign \
  --tx-body-file test-babbage2.tx \
  --signing-key-file network-files/utxo-keys/utxo2.skey \
  --testnet-magic 888 \
  --out-file test-babbage2.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file test-babbage2.signed

updatedBalance=$(getAddressBalance "$REFERENCE_INPUT_SCRIPT_ADDR")

while [ "$currentBalance" -eq "$updatedBalance" ]
do
  updatedBalance=$(getAddressBalance "$REFERENCE_INPUT_SCRIPT_ADDR")
  sleep 1
done

echo "Done!"
