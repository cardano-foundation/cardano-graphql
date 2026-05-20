#!/bin/bash

case "${NETWORK}" in
  mainnet) NETWORK_STR="--mainnet" ;;
  preprod) NETWORK_STR="--testnet-magic 1" ;;
  preview) NETWORK_STR="--testnet-magic 2" ;;
  *) echo "Unknown NETWORK: ${NETWORK}"; exit 1 ;;
esac

cmd="$1"; shift
case "$cmd" in
  cardano-node)
    exec cardano-node run \
      --socket-path /ipc/node.socket \
      --port "${CARDANO_NODE_PORT}" \
      --database-path /node/db \
      --config /config/${NETWORK}/cardano-node/config.json \
      --topology /config/${NETWORK}/cardano-node/topology.json
    ;;
  cardano-submit-api)
    exec cardano-submit-api \
      --listen-address 0.0.0.0 \
      --socket-path /ipc/node.socket \
      --port 8090 \
      ${NETWORK_STR} \
      --config /config/submit-api/config.yaml
    ;;
  *)
    echo "Unknown command: $cmd"
    exit 1
    ;;
esac
