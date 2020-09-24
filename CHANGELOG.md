# Changelog

All notable changes to this project will be documented in this file. See [standard-version](https://github.com/conventional-changelog/standard-version) for commit guidelines.

## [2.2.0](https://github.com/input-output-hk/cardano-graphql/compare/2.1.0...2.2.0) (2020-09-24)

### Compatible with:

- [`cardano-node`: `1.20.0`](https://github.com/input-output-hk/cardano-node/releases/tag/1.20.0)
- [`cardano-db-sync`: `5.0.1`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/5.0.1)

### Features
* **api-cardano-db-hasura:** add Transaction.withdrawals ([b608cc8](https://github.com/input-output-hk/cardano-graphql/commit/b608cc8475859a36e5b7deb729617e08b8a251d5))
* **client-ts:** combine graphql schemas in client package ([bd52a16](https://github.com/input-output-hk/cardano-graphql/commit/bd52a16912b7bd92de7645cca985c6157c41975a)), closes [#273](https://github.com/input-output-hk/cardano-graphql/issues/273)

## [2.1.0](https://github.com/input-output-hk/cardano-graphql/compare/2.0.0...2.1.0) (2020-09-17)

### Compatible with:

- [`cardano-node`: `1.19.1`](https://github.com/input-output-hk/cardano-node/releases/tag/1.19.1)
- [`cardano-db-sync`: `5.0.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/5.0.0) - Note: The database must be recreated using the new version.

### Features

* **api-cardano-db-hasura:** add custom $SECRET_DIR support for docker image ([ff79b10](https://github.com/input-output-hk/cardano-graphql/commit/ff79b10e9a14ec457c5515fe68c2ec27cd2207ba))
* **api-cardano-db-hasura:** add Epoch.fees ([4482338](https://github.com/input-output-hk/cardano-graphql/commit/4482338138b417eccb4cff9dd75d0bf9bd9e36af))
* **api-cardano-db-hasura:** add Transaction.deposit ([2726d80](https://github.com/input-output-hk/cardano-graphql/commit/2726d8052760aa4ccba8f679d4c938754887f61f))
* Add build arg to include genesis files ([c06912d](https://github.com/input-output-hk/cardano-graphql/commit/c06912d3203e9592e3b707c3919056acdef5a6a7))
* Improve logging during retry attempts ([e24df95](https://github.com/input-output-hk/cardano-graphql/commit/e24df95dabad423f925ca061ffd668a7f1d05be2))
* replace DB polling with postgres notification listener for migrations ([e42eb3d](https://github.com/input-output-hk/cardano-graphql/commit/e42eb3d331c9a28c71e9e1bbd1fb9aebdad35eb6))

### Bug Fixes

* add missing GraphQL model for Delegation.transaction ([3ac55b1](https://github.com/input-output-hk/cardano-graphql/commit/3ac55b13cddb1bffe7c6335a3e80f4ec96dd2b3d))
* address ordering type mismatch in GraphQL schema ([45f3c73](https://github.com/input-output-hk/cardano-graphql/commit/45f3c737b3316e87fbb5482b7f415b195f3b796a))
* ensure migration is run before introspection ([079a248](https://github.com/input-output-hk/cardano-graphql/commit/079a248eaa10de9a31f193c420988ada49661c30))
* include @types/* packages in workspace devDependencies ([6c5f075](https://github.com/input-output-hk/cardano-graphql/commit/6c5f075de8541c7983f01cc3194bcc04664b375c))
* include pools without metadata in StakePool view ([d978094](https://github.com/input-output-hk/cardano-graphql/commit/d978094dda3b504205e468822e6f42141214fd99))
* **api-cardano-db-hasura:** harden schema introspection ([3ca88b4](https://github.com/input-output-hk/cardano-graphql/commit/3ca88b4dd126ba0400ddb04e4cb0ef1fdb053b7a)), closes [#281](https://github.com/input-output-hk/cardano-graphql/issues/281)
* scope allow list to graphql path ([bee2005](https://github.com/input-output-hk/cardano-graphql/commit/bee2005730f8058d8f5363501e361b84b1ab79d8)), closes [#303](https://github.com/input-output-hk/cardano-graphql/issues/303)
* **api-cardano-db-hasura:** improve error handling with Cardano query delegation ([4e532a3](https://github.com/input-output-hk/cardano-graphql/commit/4e532a357b084b82b2a35aa7f021ba14e2744a37))
* **api-cardano-db-hasura:** Move each query to separate test ([3b29f48](https://github.com/input-output-hk/cardano-graphql/commit/3b29f484c6bb6e7b876cdce7e879d7e41037600b))
* **api-cardano-db-hasura:** Properly model and relate StakePoolRetirements ([a5fef40](https://github.com/input-output-hk/cardano-graphql/commit/a5fef4073cc3cb9f6424856e5f90aa39a31c8cf9))
* **api-cardano-db-hasura:** Support 28 byte hex encoded hashes ([9e28ffa](https://github.com/input-output-hk/cardano-graphql/commit/9e28ffa6686fd28ea72a374c4eb45b0aa7b5efe9))
* **server:** better align server config options ([30a7534](https://github.com/input-output-hk/cardano-graphql/commit/30a753435c06a06cd6fe6725bb40e9a74682010a))
* **server:** rename whitelist to allow-list ([eeeafa9](https://github.com/input-output-hk/cardano-graphql/commit/eeeafa9acb30e480e1f9cb73495eeee6574b9c5a))
* **server:** return HTTP 403 errors when rejecting disallowed queries ([b8892ca](https://github.com/input-output-hk/cardano-graphql/commit/b8892ca171164271d04018d2e039b333c74f40ee))


## 2.0.0
This new major version brings the first round of Shelley-era features to the API, 
introduces a new genesis file API package, and hardens the migrations and metadata handling. 
This version is required for transitioning through the upcoming Shelley hard fork.

### Compatible with:

- [`cardano-node`: `1.18.0`](https://github.com/input-output-hk/cardano-node/releases/tag/1.18.0)
- [`cardano-db-sync`: `3.1.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/3.1.0) - Note: The database must be recreated using the new version.

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

### Changed fields
Dates we're previously formatted to ISO 3339, however ISO 8601 is being adopted with this release for 
alignment with the Shelley genesis file format and simplification when the precision is not required. 
- `2017-10-03T21:43:51.000Z` -> `2017-10-03T21:43:51Z` 
- `Block.createdBy` -> `Block.slotLeader` links to an object, with a nullable `stakePool` field. For 
previous behaviour, `Block.slotLeader.description` can be used, however the description prefixes have
changed upstream from `SlotLeader` to `ByronGenesis`
- `Block.createdAt` -> `Block.forgedAt`
- `Block.slotWithinEpoch` -> `Block.slotInEpoch`


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
