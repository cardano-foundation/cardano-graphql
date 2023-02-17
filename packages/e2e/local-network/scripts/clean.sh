#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root_path="$(cd "$here/.." && pwd)"
cd "$root_path"

rm -rf ./network-files/node-sp*/*
rm -rf ./sockets/*
rm -rf ./config/*
rm -rf ./logs/*
