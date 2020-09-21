#!/usr/bin/env bash

set -euo pipefail
CLIENT_TS_ROOT="$(dirname "$(dirname "$(readlink -fm "$0")")")"
shx rm -rf \
  $CLIENT_TS_ROOT/api/**/*/*index.d.ts \
  $CLIENT_TS_ROOT/api/**/*/*graphql_types* \
  $CLIENT_TS_ROOT/api/*.graphql
