#!/usr/bin/env bash

# This funds a set of fixed addresses to be used during testing.
set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"

export PATH=$PWD/bin:$PATH

# 5 million tADA per wallet
AMOUNT_PER_WALLET='5000000000000'

clean() {
  rm -rf wallets-tx.raw wallets-tx.signed
}

trap clean EXIT

while [ ! -S "$CARDANO_NODE_SOCKET_PATH" ]; do
  echo "setup-wallets.sh: CARDANO_NODE_SOCKET_PATH: $CARDANO_NODE_SOCKET_PATH file doesn't exist, waiting..."
  sleep 2
done

genesisAddr=$(cardano-cli address build --payment-verification-key-file network-files/utxo-keys/utxo3.vkey --testnet-magic 888)

walletAddr1="addr_test1qpw0djgj0x59ngrjvqthn7enhvruxnsavsw5th63la3mjel3tkc974sr23jmlzgq5zda4gtv8k9cy38756r9y3qgmkqqjz6aa7"
walletAddr2="addr_test1qrml5hwl9s7ydm2djyup95ud6s74skkl4zzf8zk657s8thgm78sn3uhch64ujc7ffnpga68dfdqhg3sp7tk6759jrm7spy03k9"
walletAddr3="addr_test1qrxhyr2flena4ams5pcx26n0yj4ttpmjq2tmuesu4waw8n0qkvxuy9e4kdpz0s7r67jr8pjl9q6ezm2jgg247y9q3zpqxga37s"
walletAddr4="addr_test1qpv5muwgjmmtqh2ta0kq9pmz0nurg9kmw7dryueqt57mncynjnzmk67fvy2unhzydrgzp2v6hl625t0d4qd5h3nxt04qu0ww7k"
walletAddr5="addr_test1qr0c3frkem9cqn5f73dnvqpena27k2fgqew6wct9eaka03agfwkvzr0zyq7nqvcj24zehrshx63zzdxv24x3a4tcnfeq9zwmn7"

# Spend the first UTxO
utxo=$(cardano-cli query utxo --address "$genesisAddr" --testnet-magic 888 | awk 'NR == 3 {printf("%s#%s", $1, $2)}')

cardano-cli transaction build \
  --babbage-era \
  --change-address "$genesisAddr" \
  --tx-in "$utxo" \
  --tx-out "$walletAddr1"+"$AMOUNT_PER_WALLET" \
  --tx-out "$walletAddr2"+"$AMOUNT_PER_WALLET" \
  --tx-out "$walletAddr3"+"$AMOUNT_PER_WALLET" \
  --tx-out "$walletAddr4"+"$AMOUNT_PER_WALLET" \
  --tx-out "$walletAddr5"+"$AMOUNT_PER_WALLET" \
  --testnet-magic 888 \
  --out-file wallets-tx.raw

cardano-cli transaction sign \
  --tx-body-file wallets-tx.raw \
  --signing-key-file network-files/utxo-keys/utxo3.skey \
  --testnet-magic 888 \
  --out-file wallets-tx.signed

cardano-cli transaction submit --testnet-magic 888 --tx-file wallets-tx.signed
