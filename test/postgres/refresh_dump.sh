#!/bin/bash

# To run this script, have the mainnet cardano-docker-stack running, and ensure the block height is above 70000
# (Block height is most easily checked by checking the logs of explorer-node container)
docker exec -t --user postgres --workdir /var/lib/postgresql cardano-docker-stack_postgres_1 pg_dump -F c -d explorer -f cexplorer.pg_dump
docker cp cardano-docker-stack_postgres_1:/var/lib/postgresql/cexplorer.pg_dump test/postgres/init/cexplorer.pg_dump