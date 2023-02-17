#!/usr/bin/env bash

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
root="$(cd "$here/.." && pwd)"

cd "$root"

export PATH=$PWD/bin:$PATH

user=$(cat /run/secrets/postgres_user)
pass=$(cat /run/secrets/postgres_password)
db=$(cat /run/secrets/postgres_db)

queryResult=$(psql -d postgresql://"$user":"$pass"@"$POSTGRES_HOST":"$POSTGRES_PORT"/"$db" -c 'select epoch.no from epoch order by epoch.no DESC limit 1')
arr=(${queryResult//\\n/ })
epoch="${arr[2]}"

if [[ $epoch -lt 3 ]]; then
  exit 9
fi

exit 0