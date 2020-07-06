Changelog
=========

## 1.0.0
## Features
-  Optimised versions of some key aggregated queries: 
    - `Block.transactionsCount`
    - `Epoch.transactionsCount`
    - `Epoch.blocksCount`
- `Cardano.networkName`
- `Transaction.size`
- `Transaction.blockIndex`
- Allow explicit ordering of `Transaction.outputs` by their natural `index`
- `Cardano` now matches the postgres view, and is an improvement over the previous version which performed two queries.
- CLI tool `cgql` with commands to assist with Docker-based deployments, including init, snapshotting, and db rebuild.
- Establish relationships to easily access transaction and block information from inputs and outputs
  - `TransactionInput.transaction`
  - `TransactionInput.sourceTransaction` (where it was an output)
  - `TransactionOutput.transaction`
- `CardanoDbMeta` via `Query.cardanoDbMeta` exposes information to understand if the dataset is complete including:
 `initialized`, `syncPercentage`, and `slotDiffFromNetworkTip`. The `epoch` data is incomplete until `initialized = true`
 , which takes around 2 hours for the initial sync as of block number `4388632`. `syncPercentage` or
  `slotDiffFromNetworkTip` provides progress.
- The codebase is now modularized to enable new API segments to be added alongside as an extension, composition of services
 for more use-cases, ability to import the executable schema and host on a different server. The packages are published,
 to npm, or can be built from source:
  - @cardano-graphql/server
  - @cardano-graphql/api-cardano-db-hasura
  - @cardano-graphql/client-ts
  - @cardano-graphql/cli
  - @cardano-graphql/util-dev
  - @cardano-graphql/util
  


## Breaking changes
- PostgreSQL views are now being managed in this codebase, so either switch to the
 [new Docker image](./hasura/Dockerfile) `inputoutput/cardano-graphql-hasura`, 
 or use the Hasura CLI as demonstrated in the [entrypoint](./hasura/docker-entrypoint.sh)
 This change was needed to be compatible with the migration strategy determind by `cardano-db-sync`,
 where the migrations need to be applied on each start of the service. The custom Docker image makes it
 possible to check your own docker-compose file into source control, as it supports Docker secrets, and
 also removes the requirement to clone this source repo to get data for mounting at runtime.
- Transaction and Block IDs are now labelled as `hash`, aligning with the domain terminology.
- `Block.merkelRootHash` -> `Block.merkelRoot`
- The aggregated and known very large numbers are now typed as String. Cardano JS has utilities to work with these return values, currently limited to currency conversion.
- `Transaction.fee` previously `String`, now `BigInt`
-  Entrypoint for the service previously found in `./dist/index.js` is now `./packages/server/dist/index.js` after building.
- `Cardano.blockHeight` removed in favour of `Cardano.tip.number`, where `tip` = the most recent `Block`. This unlocks 
more information such as `slotNo`, and capability to traverse the chain etc.
- Dates are now coerced to RFC 3339 UTC date-time strings 


### Chores
- Updates to Hasura 1.2.1
- Improves CI process by consolidating the Jest snapshot files.

## 0.4.0
### Features
- `Block.nextBlock`
- `Block.epochNo` is a workaround for the less performant Block.epoch.number

## 0.3.0
### Features
- Can now scope aggregated queries to the same filters as the non-aggregated counterparts
- Improves the domain-specific input type filtering, enabling control of results for Address summaries, and to deal with the model edge-cases such as Epoch Boundary Blocks needing to be excluded in most cases.
- Adds logical operators to filter fields, allowing stacking and more powerful expressions.
- Adds Block.createdBy identifier
### Fixes
- Corrected unnecessarily nullable fields in schema.
- Removed invalid Epoch_order_by fields

### Chores
- Updates the pacakage manager to Yarn
- Runs the test suite on Jenkins with e2e assurance, deploying the service in a Docker container, and using an instance of ApolloClient to call the HTTP server. Previously the same setup suited to local development was being used, and was missing the full simulation of a real client. 

## 0.2.1
### Features
- Extends API to include support for aggregated Epoch results
- Extends API to include network metadata: `protocolConst`, `slotDuration`, and `startTime`

## 0.2.0
### Features
- Extends API to include support for aggregated results
- Optional in-memory cache
- More control over nested list results
- Adds a load test

### Fixes
-  https://github.com/input-output-hk/cardano-graphql/issues/57
 
## 0.1.6
Initial pre-production release, scoped to the Byron-era network.
