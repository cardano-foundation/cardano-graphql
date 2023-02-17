**Current node version: 1.35.4**

Run a local test network on the host.

## How it works

- 3 stake pool nodes are run in the parent process to bootstrap the network.
- The config is similar to _mainnet_, with exceptions to fit an automated testing environment (I.E faster epochs).
- The `Test...HardFork` parameters in `config.json` expedite the protocol upgrade to the latest era within a single epoch, rather than using protocol upgrade proposals.
- Once the network is ready, `mint-tokens.sh` will mint test tokens owned by the genesis address. You can modify this script to change the initial token distribution. The genesis keys will be located in `network-files/utxo-keys`.

## How to run

### Native

Supported OS:

- Linux
- MacOS

1. Run `./scripts/install.sh` to install necessary binaries.
2. Run `./scripts/make-babbage.sh` to create all the keys, certificates and genesis files.
3. Run `./scrips/start.sh` to start the nodes.
4. Run `export CARDANO_NODE_SOCKET_PATH=$PWD/network-files/node-spo1/node.sock` for `cardano-cli` to work.
5. (Optional) Run `export PATH=$PATH:$PWD/bin` so you can use `cardano-cli` instead of `./bin/cardano-cli`.
6. Check the network: `./bin/cardano-cli query tip --testnet-magic 888`

## Notable options

1. `scrips/make-babbage.json`

- `maxTxSize`: Maximum transaction size (default 16kB).
- `initialFunds`: How initial ADA is distributed.
- `epochLength`: For the epoch duration.
  
2. `templates/babbage/alonzo-babbage-test-genesis.json`

- `maxTxExUnits`: Maximum ExUnits per transaction.
- `maxBlockExUnits`: Maximum ExUnits per block.
