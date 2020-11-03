#!/usr/bin/env bash

set -euo pipefail
SEMVER_TYPE=$1
STD_ARGS="--non-interactive --no-git-tag-version --sign-git-tag --new-version ${SEMVER_TYPE}"

yarn version ${STD_ARGS} --cwd ../  && \
yarn version ${STD_ARGS} --cwd ./packages/api-cardano-db-hasura  && \
yarn version ${STD_ARGS} --cwd ./packages/cli  && \
yarn version ${STD_ARGS} --cwd ./packages/client-ts  && \
yarn version ${STD_ARGS} --cwd ./packages/server  && \
yarn version ${STD_ARGS} --cwd ./packages/util  && \
yarn version ${STD_ARGS} --cwd ./packages/util-dev
