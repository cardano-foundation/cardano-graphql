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
## Fixes
- The aggregated and known very large numbers are now typed as String. Cardano JS has utilities to work with these return values, currently limited to currency conversion.
- `cardano` query return value is accurately defined to always return an object of type `Cardano` 
- `Transaction.fee` was a String, now a `BigInt`
- Allow explicit ordering of `Transaction.input` and `Transaction.outputs` by their natural `index`

### Chores
- The PostgreSQL views have been incorperated upstream, and are no longer managed in this codebase.
- Updates to Hasura 1.0.0-beta.10
- Improves CI process by consolidating the Jest snapshot files.
- `Cardano` now matches the postgres view, and is an improvement over the previous version which performed two queries.

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
