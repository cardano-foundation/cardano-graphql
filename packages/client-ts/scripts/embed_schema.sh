#!/usr/bin/env bash

set -euo pipefail
CLIENT_TS_ROOT="$(dirname "$(dirname "$(readlink -fm "$0")")")"
PACKAGES_ROOT="$(dirname $CLIENT_TS_ROOT)"

shx cp $PACKAGES_ROOT/api-cardano-db-hasura/schema.graphql $CLIENT_TS_ROOT/api/cardano-db-hasura/ && \
shx cp $PACKAGES_ROOT/api-genesis/schema.graphql $CLIENT_TS_ROOT/api/genesis/
