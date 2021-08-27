# Changelog

All notable changes to this project will be documented in this file. See [standard-version](https://github.com/conventional-changelog/standard-version) for commit guidelines.

### [5.1.0](https://github.com/input-output-hk/cardano-graphql/compare/5.1.0-beta.1...5.1.0) (2021-08-27)

### Compatible with:

- [`cardano-node`: `1.29.0`](https://github.com/input-output-hk/cardano-node/releases/tag/1.29.0)
- [`cardano-ogmios`: `v4.0.0-beta.9`](https://github.com/CardanoSolutions/ogmios/releases/tag/v4.0.0-beta.9)
- [`cardano-db-sync`: `11.0.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/11.0.0) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### Bug Fixes

* add network suffix to Docker Compose file ([53059b9](https://github.com/input-output-hk/cardano-graphql/commit/53059b995d18dba7bdd4c32ce90d9c01b8b56b91))
* guard sync drift between cardano-db-sync and cardano-graphql ([3c9707b](https://github.com/input-output-hk/cardano-graphql/commit/3c9707bbe618cdd2ae9868347cb0f89692cc5a0e))
* properly handle error as known exception ([755c500](https://github.com/input-output-hk/cardano-graphql/commit/755c5002ea9bb87468c5d26958da82fc06ebed99))


## [5.1.0-beta.1](https://github.com/input-output-hk/cardano-graphql/compare/5.1.0-beta.0...5.1.0-beta.1) (2021-08-15)

### Compatible with:

- [`cardano-node`: `alonzo-purple-1.0.1`](https://github.com/input-output-hk/cardano-node/releases/tag/alonzo-purple-1.0.1)
- [`cardano-ogmios`: `v4.0.0-beta.6`](https://github.com/CardanoSolutions/ogmios/releases/tag/v4.0.0-beta.6)
- [`cardano-db-sync`: `alonzo-purple-1.0.1`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/alonzo-purple-1.0.1) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### Features
* support Docker pull and run to skip builds ([ee03a57](https://github.com/input-output-hk/cardano-graphql/commit/ee03a57b7045bf3947faa0df1eaa536b3c7c482a))

### Bug Fixes

* invalid cache target in compose file ([4b4dfc3](https://github.com/input-output-hk/cardano-graphql/commit/4b4dfc37a1a7c4b3729e251219ca0fb25464d8f9))


## [5.1.0-beta.0](https://github.com/input-output-hk/cardano-graphql/compare/5.0.0...5.1.0-beta.0) (2021-08-12)

### Compatible with:

- [`cardano-node`: `alonzo-purple-1.0.1`](https://github.com/input-output-hk/cardano-node/releases/tag/alonzo-purple-1.0.1)
- [`cardano-db-sync`: `alonzo-purple-1.0.1`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/alonzo-purple-1.0.1) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### Features
* feature: Alonzo data mapping ([eff1d9c](https://github.com/input-output-hk/cardano-graphql/commit/eff1d9ced4cdcd2adee6602de801feecc28451bf))
* feature: add `AlonzoGenesis`([d600dc0](https://github.com/input-output-hk/cardano-graphql/commit/d600dc01849292fe0a5006202a9be936e56d105b))
* feature: add Alonzo blocks to conditional chain following ([8d6aaf4](https://github.com/input-output-hk/cardano-graphql/commit/8d6aaf46f01eef3774e2f2efa07838c125cfd1fb))

### Bug Fixes
* Guard logic ([110deaa](https://github.com/input-output-hk/cardano-graphql/commit/110deaaba6ef0bd8b3ecbe28d4979ee26335302c))
* OperationRequiresNodeInCurrentEra -> OperationRequiresSyncedNode ([6165965](https://github.com/input-output-hk/cardano-graphql/commit/616596568e709731387c65fea7dc1cfa4cacf92d))
* startup race-condition with chain-followers ([0b15315](https://github.com/input-output-hk/cardano-graphql/commit/0b15315f4084f6cf2de1773e55a9217149c33e4b))


## [5.0.0](https://github.com/input-output-hk/cardano-graphql/compare/4.0.0...5.0.0) (2021-07-14)

### Compatible with:

- [`cardano-node`: `1.27.0`](https://github.com/input-output-hk/cardano-node/releases/tag/1.27.0)
- [`cardano-db-sync`: `10.0.1`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/10.0.1) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### ⚠ BREAKING CHANGES

* `Asset.assetName` and `Asset.assetId` are now typed as `Hex`, as the relationship from `Assets` to `TokenMints` needed
  to be established using the underlying table, where `assetId` is not present.
* Configuration of the asset metadata fetching is now a single value. `POLLING_INTERVAL_METADATA_SYNC_INITIAL` and
  `POLLING_INTERVAL_METADATA_SYNC_ONGOING` are replaced by `ASSET_METADATA_UPDATE_INTERVAL`, which is the number of
  seconds before the service checks the registry for updates.

* AssetSupply.total is now an optional field, and can return null.
* Transaction.metadata is now `JSON` type, not `JSONObject`
  - Despite the name, `JSONObject` was mapped to the underlying JSON resolver as a workaround to avoid breaking changes.
* The fields previously modelled on Token have been nested under Token.asset
* `tokens` and `tokens_aggregate` have been removed in favour of assets and assets_aggregate
- Asset properties nested under `PaymentAddress.assets` are
  now under `PaymentAddress.assets.asset`
* `Block.merkleRoot` removed, as no longer part of `cardano-db-sync` schema. ([8b3b718](https://github.com/input-output-hk/cardano-graphql/commit/8b3b7185f619cc6ede85ac18b43c96fdb76ba19e))

### Features
* Replaces the use of `cardano-cli` with [Ogmios](https://github.com/CardanoSolutions/ogmios/) for interacting with the
  node using JSON-WSP, and implements the convenient `cardano-node-ogmios` Docker image as a drop-in replacement for
  the `cardano-node` image.
* Sync assets using chain-sync protocol via Ogmios ([784509e](https://github.com/input-output-hk/cardano-graphql/commit/784509eed509dc7bfd856aa4ee9cea7a7b40b046))
* `tokenMints` and `tokenMints_aggregate` queries ([f803b94](https://github.com/input-output-hk/cardano-graphql/commit/f803b94f67fd016c47d0832510f7af9628a2186c))
* add query complexity validations ([24a14a9](https://github.com/input-output-hk/cardano-graphql/commit/24a14a9cde019d4232fc9abf5cb4c6dcbf37638f)), 
* add default query complexity calculations with arguments ([b16c77d](https://github.com/input-output-hk/cardano-graphql/commit/b16c77d0411696736ed3028f53925c8fa320780f))
* implement complexity in resolvers ([5664a55](https://github.com/input-output-hk/cardano-graphql/commit/5664a5505c52168de0b343ab81f5350a31e99bb3))
* use cardano-node config for lastConfiguredMajorVersion ([5a621c2](https://github.com/input-output-hk/cardano-graphql/commit/5a621c2d1f16ec90693ee57b1c67d7d56b6ece33)), closes [#454](https://github.com/input-output-hk/cardano-graphql/issues/454)
* add Epoch_bool_exp.startedAt ([4cc580a](https://github.com/input-output-hk/cardano-graphql/commit/4cc580accc7eb7ae09e608ab1023e6a70cda1206))
* add TransactionOutput_bool_exp.index ([34d5438](https://github.com/input-output-hk/cardano-graphql/commit/34d543899bba5f325e52bac67301d6baab01c1f8))
* allow ordering of assets by token mint count ([cfbb039](https://github.com/input-output-hk/cardano-graphql/commit/cfbb03921ea65fe80a1132e433acc4a534e54544))
* Asset metadata decimals ([082f509](https://github.com/input-output-hk/cardano-graphql/commit/082f509d4c88a387ed20c587cf1160d291db454a))

### Bug Fixes
* improve startup robustness ([7054a59](https://github.com/input-output-hk/cardano-graphql/commit/7054a596448e879ea8d113209422ad6f1c46e990))
  - The GraphQL server is now delayed until all modules are initialized, and running. If `cardano-db-sync` reboots, all 
    modules are shutdown, and restarted and therefore links the server availability with a fully initialized stack.
* Add order_by to asset query ([5848c24](https://github.com/input-output-hk/cardano-graphql/commit/5848c24d533fe52ce7a89b3c66cb1096b54baaec))
* Asset_bool_exp.tokenMints ([7616a49](https://github.com/input-output-hk/cardano-graphql/commit/7616a49eda5c165c4fc4fa922e2c3da72971e4d2))
* assetFingerprint bug when no assetName present ([9a61d91](https://github.com/input-output-hk/cardano-graphql/commit/9a61d91f94a4006f9b1aef29e0ff498f670d806d)), closes [#487](https://github.com/input-output-hk/cardano-graphql/issues/487)
* await all promises in shutdown function ([3d7ef11](https://github.com/input-output-hk/cardano-graphql/commit/3d7ef110306834545dea74632e974d36149251a6))
* await CardanoNodeClient init before starting server ([f33f6aa](https://github.com/input-output-hk/cardano-graphql/commit/f33f6aaae1b9af3f4800d617e473cf52c58fc6f9))
* cardanoDbSyncMeta.initialized during startup ([d6649cf](https://github.com/input-output-hk/cardano-graphql/commit/d6649cfd0b88f6438340b76b819b5b031b082166))
* casing on invalid hereafter in GraphQL schema ([854ec4a](https://github.com/input-output-hk/cardano-graphql/commit/854ec4a9c096206ff260a29227b107d94c374159)), closes [#390](https://github.com/input-output-hk/cardano-graphql/issues/390)
* Copy cardano-node config into Docker image ([4bf4866](https://github.com/input-output-hk/cardano-graphql/commit/4bf4866f3252d8e3c93143dd3a91a767b68a9c47))
* default metadata server URI ([2a98055](https://github.com/input-output-hk/cardano-graphql/commit/2a980552b9194eb38a099849535cef16344cf6d9))
* disable CORS in Hasura graphql-engine ([4499422](https://github.com/input-output-hk/cardano-graphql/commit/449942288ae17eed53d8438cfcd04f28921edc45)), closes [#392](https://github.com/input-output-hk/cardano-graphql/issues/392)
* Ensure DB is in current era before completing HasuraClient initialization ([0d43abc](https://github.com/input-output-hk/cardano-graphql/commit/0d43abce1ca51760e0f620dbc270a04d0df76086))
* guards on missing current epoch row ([6204c96](https://github.com/input-output-hk/cardano-graphql/commit/6204c96835c1d0c704b582f055a93c53465123e6))
* log all error messages during service startup, not just `HostDoesNotExist` ([c81aad8](https://github.com/input-output-hk/cardano-graphql/commit/c81aad883e6532893d8563d78acc009f5415b234))
* memory leak in Hasura GraphQL client ([3c8511d](https://github.com/input-output-hk/cardano-graphql/commit/3c8511d93674e97a01fd2502cb2090c7c6966806))
* move current era check to be lazy operation ([7830c91](https://github.com/input-output-hk/cardano-graphql/commit/7830c912cc583dd4e66fac30465cea60d7f1c7dd))
* move graphql-engine option under serve command ([f76e944](https://github.com/input-output-hk/cardano-graphql/commit/f76e944b70c3419ce5c4b56c6a35cdd904afccf0))
* policyId comparison expression type ([99318cc](https://github.com/input-output-hk/cardano-graphql/commit/99318ccc76c5eb4cef9c205124612d36d9a0069a)), closes [#485](https://github.com/input-output-hk/cardano-graphql/issues/485)
* remove promise chaining cycle ([8b55ca0](https://github.com/input-output-hk/cardano-graphql/commit/8b55ca00d7ff061a39cadf92f3da5f5cd6c7072d)), closes [#459](https://github.com/input-output-hk/cardano-graphql/issues/459)
* Reorder logging arguments based on  Bunyan interface ([46b26a0](https://github.com/input-output-hk/cardano-graphql/commit/46b26a05139d12894ed5a0f58ccbdc73583d2706))
* Restore protocol params fetcher init ([e86fc1a](https://github.com/input-output-hk/cardano-graphql/commit/e86fc1a734b3b1bec259bebf5cdb8da1d747b437))
* scoping on package ([82e2040](https://github.com/input-output-hk/cardano-graphql/commit/82e204030d9ec09576bc61a85c08d641a0ba3a3b))
* submitTransaction error mapping ([3629725](https://github.com/input-output-hk/cardano-graphql/commit/3629725e87a981a94a8fc66658ebcf86d5c369c5))

## [4.0.0](https://github.com/input-output-hk/cardano-graphql/compare/3.0.1...4.0.0) (2021-03-29)

### Compatible with:

- [`cardano-node`: `1.25.1`](https://github.com/input-output-hk/cardano-node/releases/tag/1.25.1)
- [`cardano-db-sync`: `9.0.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/9.0.0) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### ⚠ BREAKING CHANGES

* StakePool.url no longer URL type. The ledger does not validate the url value provided by the owner,
therefore trying to impose rules around the structure will fail.

* Transaction.metadata is now more accurately JSON type, not JSONObject. Despite the name, 
  JSONObject was currently mapped to the underlying JSON resolver as a workaround to avoid breaking changes.

* The fields previously modelled on Token have been nested under Token.asset. tokens and tokens_aggregate 
  have been removed in favour of assets and assets_aggregate. Asset properties are now nested under 
  PaymentAddress.summary.assetBalances.asset

### Bug Fixes

* ActiveStake.stakePoolHash field name ([08fe609](https://github.com/input-output-hk/cardano-graphql/commit/08fe609f6d027fe8ce62e8feeac6d7f4a4df905b))
* add Mint.asset relationship ([0deaaac](https://github.com/input-output-hk/cardano-graphql/commit/0deaaac3cce76268eacd3faa64e1894cfa8cae36))
* add remaining Asset fields Hasura query used to fulfil paymentAddress query ([879db94](https://github.com/input-output-hk/cardano-graphql/commit/879db9410801668ab3e6e8507898225585b70782))
* batch asset synchronising operations ([637d607](https://github.com/input-output-hk/cardano-graphql/commit/637d6077748cfc4264485b18e64136a4f34b2265))
* Block.merkelRoot -> Block.merkleRoot ([b9e1e13](https://github.com/input-output-hk/cardano-graphql/commit/b9e1e13fdbca32b62618a4ca6f27bfed271e3fc9))
* include rewards in ada circulating supply ([69f23c1](https://github.com/input-output-hk/cardano-graphql/commit/69f23c1c114ede071efd7c68864e706715c4d2a1))
* StakePool.url is now type String ([a661525](https://github.com/input-output-hk/cardano-graphql/commit/a661525db77db020ea87132cbdfd4e658ebdc4db))
* StakePoolRetirement_bool_exp.inEffectFrom type ([96aa708](https://github.com/input-output-hk/cardano-graphql/commit/96aa7088de16134808758fc97775569121ab253b))
* Correct Transaction.metadata type ([79205a2](https://github.com/input-output-hk/cardano-graphql/commit/79205a209b3d511fd05b8ad8e33e1e3d02ac53da))
* Remove cardano-cli ledger-state query, make AssetSupply.total optional ([f86ef42](https://github.com/input-output-hk/cardano-graphql/commit/f86ef42298574785e29435265f622fef5fa57129))
* use cardano-node config for lastConfiguredMajorVersion ([5a621c2](https://github.com/input-output-hk/cardano-graphql/commit/5a621c2d1f16ec90693ee57b1c67d7d56b6ece33)), closes [#454](https://github.com/input-output-hk/cardano-graphql/issues/454)
* replace cardano-cli query with db lookup for protocol params ([a6387b3](https://github.com/input-output-hk/cardano-graphql/commit/a6387b381c8c63d2110d02accb10fdda10937b62))
* throw error in resolver rather than return it ([dd9a031](https://github.com/input-output-hk/cardano-graphql/commit/dd9a031675e9cc708f2bb62b5cad1b454f74b14a))

### Features

* Introduce asset model and fetch metadata from external service ([0329a84](https://github.com/input-output-hk/cardano-graphql/commit/0329a84c7a137cc8135635afd10502243324208a))
* Ada Pots ([9c7f058](https://github.com/input-output-hk/cardano-graphql/commit/9c7f058ef331a06576ad5cf5e567af4ddb723bf0))

## [3.2.0](https://github.com/input-output-hk/cardano-graphql/compare/3.1.1...3.2.0) (2021-02-01)

- [`cardano-node`: `1.25.1`](https://github.com/input-output-hk/cardano-node/releases/tag/1.25.1)
- [`cardano-db-sync`: `8.0.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/8.0.0) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### Features
* Ada supply query ([e716d51](https://github.com/input-output-hk/cardano-graphql/commit/e716d51b801fa758b26843f3853949d0d3c434a7))
* Submit transaction mutation ([29f1adb](https://github.com/input-output-hk/cardano-graphql/commit/29f1adb0b3a2c3762dc0dd8dadb4bff4a3041245))
* Multi-asset support ([86e4206](https://github.com/input-output-hk/cardano-graphql/commit/86e42064a02e1aabb6e4b2ea1d53157b4e21a7ef))
* Structured logging ([f948cf5](https://github.com/input-output-hk/cardano-graphql/commit/f948cf51ca3d2e95147e219cbed3323a6c3bf0d0))
* Improve delegation and active stake models ([ad06274](https://github.com/input-output-hk/cardano-graphql/commit/ad062749e8250c0ddbb9139539aeeed31389281c))
* Report sync progress in logs during initialisation ([83279c3](https://github.com/input-output-hk/cardano-graphql/commit/83279c35a7b96d8b7abe2f09e4e99fc50d731d7b))

### Bug Fixes

* GraphQL schema order_by ([9869a48](https://github.com/input-output-hk/cardano-graphql/commit/9869a481e0e0ac4022da9d1ad842fccb60feb272))
* guard against simultaneous data fetches ([93e198f](https://github.com/input-output-hk/cardano-graphql/commit/93e198f900b697fcbd82a939d30bbd9cb8486094))
* Opt-out of Hasura CLI telemetry ([196086c](https://github.com/input-output-hk/cardano-graphql/commit/196086ce9dc0f5565d34c676e5a4ccda77726f17))
* pass logger to onFailedAttempt ([1b8b0cf](https://github.com/input-output-hk/cardano-graphql/commit/1b8b0cf3b651a4d7b9b4bbb14409d2a523b95f74))
* Replaces the time-based logic to determine sync progress and initialisation state ([b29ef3e](https://github.com/input-output-hk/cardano-graphql/commit/b29ef3e31820eadcb876278f0c8e9653d387a16b)), closes [#248](https://github.com/input-output-hk/cardano-graphql/issues/248)

### [3.1.1](https://github.com/input-output-hk/cardano-graphql/compare/3.1.0...3.1.1) (2020-12-21)

- [`cardano-node`: `1.24.2`](https://github.com/input-output-hk/cardano-node/releases/tag/1.24.2)
- [`cardano-db-sync`: `7.1.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/7.1.0) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### Bug Fixes

* JSON resolver for transaction metadata ([9085036](https://github.com/input-output-hk/cardano-graphql/commit/9085036dca3abead111ad3cb1ab57389affe2939)), closes [#389](https://github.com/input-output-hk/cardano-graphql/issues/389)


### [3.1.0](https://github.com/input-output-hk/cardano-graphql/compare/3.0.1...3.1.0) (2020-12-10)

### Compatible with:

- [`cardano-node`: `1.24.2`](https://github.com/input-output-hk/cardano-node/releases/tag/1.24.2)
- [`cardano-db-sync`: `7.1.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/7.1.0) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### Features
* Transaction validity interval fields ([ad9b4e5](https://github.com/input-output-hk/cardano-graphql/commit/ad9b4e5d3aebc2ca0984c67114e53f7f31210ec2))
  - `Transaction.invalidBefore`
  - `Transaction.invalidHereafter`
  - associated input fields
### Bug Fixes

* Reward and StakeDeregistration order_by fields ([a6f9a88](https://github.com/input-output-hk/cardano-graphql/commit/a6f9a885b507b8b4da32e257b4230765fa27e855)), closes [#382](https://github.com/input-output-hk/cardano-graphql/issues/382)


### [3.0.1](https://github.com/input-output-hk/cardano-graphql/compare/3.0.0...3.0.1) (2020-11-27)

### Compatible with:

- [`cardano-node`: `1.21.1`](https://github.com/input-output-hk/cardano-node/releases/tag/1.21.1)
- [`cardano-db-sync`: `6.0.1`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/6.0.1) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.3`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.3)

### Bug Fixes
* add missing comparison operator fields to bool expressions ([89addef](https://github.com/input-output-hk/cardano-graphql/commit/89addef432c79d300a3935a72a066bbaa529acb2))
* StakePool_bool_exp.rewards type ([1f02a68](https://github.com/input-output-hk/cardano-graphql/commit/1f02a68448946c6a1eda6213396ff0fca4379fbb))

## [3.0.0](https://github.com/input-output-hk/cardano-graphql/compare/2.2.0...3.0.0) (2020-11-05)
This new major version, now based on the current Node.js LTS, brings the second round of Shelley-era 
features to the API. Most notably, rewards, active stake captured at each epoch boundary, 
transaction metadata, protocol parameters in effect during the epoch, and custom types for the Bech32 
values covered by [CIP5](https://github.com/cardano-foundation/CIPs/tree/master/CIP5). 

You may be impacted by breaking changes, which are listed below.


### Compatible with:

- [`cardano-node`: `1.21.1`](https://github.com/input-output-hk/cardano-node/releases/tag/1.21.1)
- [`cardano-db-sync`: `6.0.0`](https://github.com/input-output-hk/cardano-db-sync/releases/tag/6.0.0) - Note: The database must be recreated using the new version.
- [`hasura/graphql-engine`: `1.3.2`](https://github.com/hasura/graphql-engine/releases/tag/v1.3.2)

### ⚠ BREAKING CHANGES

* Omitting the limit in queries will now return 2500 records, rather than 100
* `WHITELIST_PATH` is now `ALLOW_LIST_PATH`
* `HASURA_CLI_PATH` is now required configuration, however it's set in the Dockerfile and nix service by default.
* All schema instances for stake addresses are now validated for the correct prefix using a custom scalar
* `Block.vrfKey` and the associated input types are now a validated for the correct prefix using a custom scalar
* `StakePool.id` and the associated input types are now a validated for the correct prefix using a custom scalar
* `Hash32HexString` -> `Hash32Hex`
* `Hash32HexString_comparison_exp` -> `Hash32Hex_comparison_exp`
* `SlotLeader.hash`, `StakePool.hash`, `StakePoolOwner.hash` are now `Hash28Hex` type
* `Block.vrfKey` is now the bech32 encoded value as per CIP5. It's now typed as `VRFVerificationKey`. 

### Features
* Rewards ([46cc878](https://github.com/input-output-hk/cardano-graphql/commit/46cc87880fed5118560c76dd62e7b038eb5df689))
  - top-level queries rewards and rewards_aggregate
  - StakePool.rewards and StakePool.rewards_aggregate
* `StakePool.id` ([3fc763c](https://github.com/input-output-hk/cardano-graphql/commit/3fc763c37e7dc96c1b36191ec85f40c992d47221))
  - Bech32 encoded, prefixed with `pool`
* Introduce ActiveStake concept ([1b66fee](https://github.com/input-output-hk/cardano-graphql/commit/1b66fee2f54c4a4995d2e407693aaf7e470c5cfb))
  - Snapshot of the stake linked to a registered stake key, per epoch.
  - This features adds an activeStake query, and extends the Epoch type to include activeStake and activeStake_aggregate fields.
  - top-level activeStake_aggregate query ([8e13a52](https://github.com/input-output-hk/cardano-graphql/commit/8e13a5267fe89c409d17de1337cf2fab88dab7a7))
* `Epoch.protocolParams` and `Epoch.nonce` ([c08d652](https://github.com/input-output-hk/cardano-graphql/commit/c08d652f4b584e757726710626eef0610154da07))
* `Transaction.metadata` ([8cec6e8](https://github.com/input-output-hk/cardano-graphql/commit/8cec6e8fde66979075f18d53aa1c1e46d4d61085))
* Custom GraphQL scalars
  - `StakeAddress` ([1bd5c8b](https://github.com/input-output-hk/cardano-graphql/commit/1bd5c8b41bddb575570b539f8344c1fb415b3598))
  - `StakePoolID` ([627db37](https://github.com/input-output-hk/cardano-graphql/commit/627db37d33bcda2b955100bd654fe6c84983d690))
  - `VRFVerificationKey` ([eae8f72](https://github.com/input-output-hk/cardano-graphql/commit/eae8f726d9742ae39248cf6a92b91adb5b7438f5))

### Improvements
* consolidate `api-genesis` with `api-cardano-db-hasura`([0f927c4](https://github.com/input-output-hk/cardano-graphql/commit/0f927c45ce50296ac176384380f68275561c4d2b))
* upgrade to Node.js 14 ([950852a](https://github.com/input-output-hk/cardano-graphql/commit/950852a1b88596d4b0e5ad54b05a24d79acfd7a4))
* Require explicit path to hasura CLI executable ([ca22d42](https://github.com/input-output-hk/cardano-graphql/commit/ca22d423e840290946b0f1c7b4d88461c0b7d2e3))
* Remove depreciated config option `WHITELIST_PATH` ([a2d00ff](https://github.com/input-output-hk/cardano-graphql/commit/a2d00ff2e0c6cb1568820990debea55e06526fd2))
* Change Hasura record limit from 100 to 2500 ([212bc47](https://github.com/input-output-hk/cardano-graphql/commit/212bc4731f66af66d4eb49ed68c7e2e847423cfd)), closes [#333](https://github.com/input-output-hk/cardano-graphql/issues/333)
* server configuration lifted to Dockerfile ([167ff42](https://github.com/input-output-hk/cardano-graphql/commit/167ff426a8934cfda23d88089082d01331e04f64))
* expose the API port (Removes the hard-coding) ([e422734](https://github.com/input-output-hk/cardano-graphql/commit/e422734c2e412976aac976daecc10cb5f3779e99))

### Bug Fixes
* accurately allocate hashes to 28 byte type ([f9597bc](https://github.com/input-output-hk/cardano-graphql/commit/f9597bc3ffb595aa74566c04c08b4c2c3a724506))
* add port and custom column names to pool_relay mapping ([879a51a](https://github.com/input-output-hk/cardano-graphql/commit/879a51ac3bb2a7f11837e410678f558e3773ed5e)), closes [#328](https://github.com/input-output-hk/cardano-graphql/issues/328) [#329](https://github.com/input-output-hk/cardano-graphql/issues/329)
* Change Block.vrfKey type to String ([a34c8aa](https://github.com/input-output-hk/cardano-graphql/commit/a34c8aa42899cd2aec2541183933ab8f511b0963))
* properly type ShelleyProtocolParams ([a7a2c3a](https://github.com/input-output-hk/cardano-graphql/commit/a7a2c3acdf0272207e61b2aacf4ec7565abdd06b))
* remove invalid Genesis.shelley.protocolMagicId ([a73ba1e](https://github.com/input-output-hk/cardano-graphql/commit/a73ba1e56ae543b65376766a6f898f81e4ee3e05)), closes [#327](https://github.com/input-output-hk/cardano-graphql/issues/327)
* remove StakePool.withdrawals field ([46dd937](https://github.com/input-output-hk/cardano-graphql/commit/46dd93720428fa28d45e288c7c625dc2b0b8210b))
* typo in missing config error message ([d904a72](https://github.com/input-output-hk/cardano-graphql/commit/d904a72fdc80ee3b3e8dc0eea0cf1b1bf610389f))

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
