Changelog
=========

## 2.0.0
This new major version brings the first round of Shelley-era features to the API, 
introduces a new genesis file API package, and hardens the migrations and metadata handling. 
This version is required for transitioning through the upcoming Shelley hard fork.

### Compatible with:

- [`cardano-node`: `1.18.0`](https://github.com/input-output-hk/cardano-node/releases/tag/1.18.0)
- [`cardano-db-sync`: `3.0.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/3.0.0) - Note: The database must be recreated using the new version.

## Features
### New Queries
- `stakePools`, `stakePools_aggregate`
- `delegations`, `delegations_aggregate`
- `stakeRegistrations`, `stakeRegistrations_aggregate`
- `stakeDeregistrations`, `stakeDeregistrations_aggregate`
- `withdrawals`, `withdrawals_aggregate`
- `genesis`
- Metadata and SQL migrations are now performed within the application layer, and make the service immune to schema
being removed should `cardano-db-sync` restart. using the 
[Hasura CLI](https://hasura.io/docs/1.0/graphql/manual/hasura-cli/install-hasura-cli.html), which
is included in the [Dockerfile](./Dockerfile) and [NixOS](./nix/nixos/cardano-graphql-service.nix) 
service, however outside of this you must install and place `hasura` on PATH. 
- A new API package [`@cardano-graphql/api-genesis`](./packages/api-genesis/README.md) allows 
access to the network genesis files. It's integrated into the server, with the config exposed 
as environment variables. As usual, the docker-compose.yaml serves as a good reference.

## Breaking Changes :warning:
- The docker-compose file now mounts configuration managed in the repository, restoring the usual 
separation of concerns with service configuration. The Docker images still have the 
configuration included at build time, however in practice, being ready to managing your own 
configuration if required is a good strategy. Simply copying the top level `config` and committing
 to source control gives you full control over the services using their native interface.  
### Removed fields
- `Cardano.networkName` **removed**. Use network magic from the genesis API identify networks.
- `Cardano.protocolConst`, `Cardano.slotDuration`, `Cardano.startTime`, `Cardano.slotsPerEpoch`
**removed**. Access this info from the Genesis API.
- `cardanoDbSync.slotDiffFromNetworkTip` **removed** in reponse to a change in strategy for determining 
sync status with `cardano-db-sync` determining sync status relies on a chain
that has produce
- `Block.slotWithinEpoch` **removed** due to complexity with variation across eras. The Genesis API has information
for calculations based on context.

### Changed fields
Dates we're previously formatted to ISO 3339, however ISO 8601 is being adopted with this release for 
alignment with the Shelley genesis file format and simplification when the precision is not required. 
- `2017-10-03T21:43:51.000Z` -> `2017-10-03T21:43:51Z` 
- `Block.createdAt` -> `Block.forgedAt`
- `Block.createdBy` -> `Block.slotLeader` links to an object, with a nullable `stakePool` field. For 
previous behaviour, `Block.slotLeader.description` can be used, however the description prefixes have
changed upstream from `SlotLeader` to `ByronGenesis`

## Chores
- Migrations have been squashed into a single step.

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
- Packages are now managed via an offline npm package cache, to facilitate reproducible builds when the --offline flag 
is passed to yarn install
  


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
