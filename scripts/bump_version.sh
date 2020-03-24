#!/usr/bin/env bash

set -euo pipefail
SEMVER_TYPE=$1
STD_ARGS="--non-interactive --no-git-tag-version --sign-git-tag --new-version ${SEMVER_TYPE}"

# cardano-graphql
yarn version ${STD_ARGS} && \
# cardano-graphql-cli
yarn version ${STD_ARGS} --cwd ./cli  && \
# cardano-graphql-ts
yarn version ${STD_ARGS} --cwd ./generated_packages/TypeScript
