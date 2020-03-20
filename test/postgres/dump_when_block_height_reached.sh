#!/bin/bash

HTTP_SERVER_URI=$1
TARGET_BLOCK_HEIGHT="70000"
DB_NAME=$2
FILE_NAME='cexplorer.pg_dump'
OUT_PATH="test/postgres/init/${FILE_NAME}"

printf "\nPolling $HTTP_SERVER_URI until block height exceeds $TARGET_BLOCK_HEIGHT\n"
while true;
do
	blockheight=$(curl -s -X POST -H "Content-Type: application/json" -d '{"query": "{ cardano { blockHeight }}"}' "$HTTP_SERVER_URI" | jq ".data.cardano.blockHeight");
	if [ ! $blockheight = 'null' ]; then
	  printf "\r$(date +%H:%M:%S): $blockheight";
    if [ "${blockheight}" -gt "${TARGET_BLOCK_HEIGHT}" ]; then
      break;
    fi;
  fi;
	sleep 1;
done

docker exec -t --user postgres --workdir /var/lib/postgresql cardano-graphql_postgres_1 pg_dump -F c -d ${DB_NAME} -f ${FILE_NAME}
docker cp cardano-graphql_postgres_1:/var/lib/postgresql/${FILE_NAME} ${OUT_PATH}
printf "\nA new PostgreSQL dump has been written to ${OUT_PATH}\n"