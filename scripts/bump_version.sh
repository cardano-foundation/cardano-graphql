#!/usr/bin/env bash

set -euo pipefail
VERSION=$1
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="${SCRIPT_DIR}/.."
STD_ARGS="--non-interactive --no-git-tag-version --sign-git-tag --new-version ${VERSION}"

yarn version ${STD_ARGS} --cwd ${ROOT_DIR}
yarn version ${STD_ARGS} --cwd ${ROOT_DIR}/packages/api-cardano-db-hasura
yarn version ${STD_ARGS} --cwd ${ROOT_DIR}/packages/cli
yarn version ${STD_ARGS} --cwd ${ROOT_DIR}/packages/client-ts
yarn version ${STD_ARGS} --cwd ${ROOT_DIR}/packages/server
yarn version ${STD_ARGS} --cwd ${ROOT_DIR}/packages/util
yarn version ${STD_ARGS} --cwd ${ROOT_DIR}/packages/util-dev

# Update @cardano-graphql/* dependency versions across all packages
node -e "
const fs = require('fs');
const path = require('path');
const pkgDir = path.join('${ROOT_DIR}', 'packages');
fs.readdirSync(pkgDir).forEach(dir => {
  const pkgPath = path.join(pkgDir, dir, 'package.json');
  if (!fs.existsSync(pkgPath)) return;
  const pkg = JSON.parse(fs.readFileSync(pkgPath, 'utf8'));
  let changed = false;
  ['dependencies', 'devDependencies'].forEach(section => {
    if (!pkg[section]) return;
    Object.keys(pkg[section]).forEach(dep => {
      if (dep.startsWith('@cardano-graphql/')) {
        pkg[section][dep] = '${VERSION}';
        changed = true;
      }
    });
  });
  if (changed) fs.writeFileSync(pkgPath, JSON.stringify(pkg, null, 2) + '\n');
});
"
