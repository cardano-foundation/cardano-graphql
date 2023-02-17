#!/usr/bin/env bash

# This script updates SP5: This pool has a positive pledge (>0), meets the pledge and has metadata (shares name with SP4).
set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/../.." && pwd)"
cd "$root"
export PATH=$PWD/bin:$PATH

# pool parameters
SP_NODE_ID=5
POOL_PLEDGE=410000000
POOL_OWNER_STAKE=500000000 # Must be greater than pledge
POOL_COST=390000000
POOL_MARGIN=0.15
METADATA_URL="http://file-server/SP${SP_NODE_ID}.json"

source ./scripts/pools/update-node-utils.sh

trap clean EXIT

updatePool ${SP_NODE_ID} ${POOL_PLEDGE} ${POOL_OWNER_STAKE} ${POOL_COST} ${POOL_MARGIN} ${METADATA_URL}
