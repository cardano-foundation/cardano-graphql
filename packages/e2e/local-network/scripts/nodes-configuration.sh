#!/usr/bin/env bash

AMOUNT_PER_NODE='10000000000000'
NUM_SP_NODES=11
SP_NODES_ID=$(seq 1 ${NUM_SP_NODES})
SP_NODES=$(for i in ${SP_NODES_ID} ; do echo "node-sp${i}" ; done)
