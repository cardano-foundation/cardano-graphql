# After DB Changes
## 1. Make a new snapshot

The project uses a small snapshot from the mainnet as the development and test dataset, enabling
concrete assertions to remain mostly static. Refresh this dataset if there are changes to the 
PostgreSQL schema of `cardano-db-sync-extended`. 

### Warning
**The script will remove any existing volumes associated with the mainnet stack**

With [jq](https://stedolan.github.io/jq/download/) installed
```
yarn create-db-snapshot
```
Run tests, update assertions and Jest snapshots after verifying against a 3rd party trusted API 
(from another implementation).

## 2. Update Hasura 

[Official Documentation](https://docs.hasura.io/1.0/graphql/manual/index.html)

When the Postgres data source or view abstractions change, you **_may_** need to update the metadata defined in `hasura/metadata.json`.

1. Make the required metadata changes through the Hasura Console.
2. Export the new metadata through the settings utility.
3. Commit the changes to `hasura/metadata.json` as part of the upgrade.
