# DB Snapshot

The project uses a small snapshot from the mainnet as the development and test dataset, enabling
concrete assertions to remain mostly static. You will need to refresh this dataset if there are
changes to `cardano-db-sync-extended`. 

## Warning
**The script will remove any existing volumes associated with the mainnet stack**

1. Ensure you have [jq](https://stedolan.github.io/jq/download/) installed
2. `yarn create-db-snapshot`
3. Once complete, the new dev/test database dump will overwrite the previous. Run tests, update new valid assertions and Jest snapshots.
4. Commit changes. 