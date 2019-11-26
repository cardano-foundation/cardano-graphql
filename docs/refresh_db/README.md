# Refresh Test DB Dump

1. clone `cardano-docker-stack` from GH
2. From cardano-docker-stack, run `./scripts/start_mainnet.sh`
3. Once the stack starts, use `docker ps` to identify `cardano-explorer` container. Then use `docker logs -f _container_name_` to monitor the block height as the explorer receives and writes blocks.
4. Once block height is above 31070, run `yarn refresh-db` from the root directory of `cardano-graphql`

Note: It is _expected_ that some of the snapshot values will change and need to be updated, as you will not dump the db at the exact same block as the last dump