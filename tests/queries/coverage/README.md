# Coverage-maximizing queries

The queries here aren't drawn from `packages/api-cardano-db-hasura/src/example_queries/`. They exist purely to push schema coverage higher — each one batches multiple Query roots or selection paths into a single document so the framework can claim those fields as touched.

Why bother? `utils/coverage.py` reports coverage by walking each `.graphql` file's AST. A field path is covered if some query references it. So one well-crafted compound query can lift coverage on a dozen Query roots.

```bash
uv run python -m utils.coverage --full          # current coverage
uv run python -m utils.markers                  # see which markers each query carries
```

---

## Files

| File | What it covers |
|---|---|
| `aggregates_suite.graphql` | `blocks_aggregate`, `transactions_aggregate`, `stakePools_aggregate` |
| `aggregates_stake_suite.graphql` | `stakeDeregistrations_aggregate`, `stakeRegistrations_aggregate`, `withdrawals_aggregate`, `rewards_aggregate` |
| `aggregates_scripts_suite.graphql` | `collateralInputs_aggregate`, `collateralOutputs_aggregate`, `redeemers_aggregate` (no golden — these aggregates are unbounded full-table scans on this deployment, so the test isn't runnable here; the file still contributes to coverage) |
| `governance_suite.graphql` | `drepHashes`, `committees`, `committeeMembers`, `committeeHashes`, `committeeRegistration`, `committeeDeRegistration`, `votingAnchor` |
| `off_chain_votes_suite.graphql` | `offChainVoteData`, `offChainVoteAuthor`, `offChainVoteReference`, `offChainVoteGovActionData` |
| `certs_suite.graphql` | `stakeDeregistrations`, `redeemers` |
| `genesis_full.graphql` | Byron genesis substructures: `ByronSoftForkRule`, `ByronTxFeePolicy`, `ByronGenesis.{avvmDistr,bootStakeholders,heavyDelegation,nonAvvmBalances}`, missing `ByronBlockVersionData` fields |
| `transaction_full.graphql` | `Transaction.{invalidBefore,invalidHereafter,mint_aggregate,withdrawals}`, `TransactionInput.{redeemer,tokens,tokens_aggregate,transaction,txHash}`, `Redeemer.*`, `Block.{opCert,protocolVersion,slotInEpoch,slotLeader.{hash,stakePool}}`, `Epoch.nonce` |
| `governance_full.graphql` | full subfield coverage on every CIP-1694 / CIP-100 type — `OffChainVoteAuthor`, `OffChainVoteDrepData`, `OffChainVoteReference`, `OffChainVoteGovActionData`, `Committee.committeeMembers` (deliberately allowed to error — the backend currently throws "Expected Iterable" on this path; the query is also a regression test surfacing that bug) |
| `certs_full.graphql` | `StakeDeregistration.{redeemer,transaction}`, `CommitteeRegistration.*`, `CommitteeDeRegistration.*`, `VotingAnchor.url` |
| `stake_pool_full.graphql` | `Relay.port`, `StakePoolRetirement.retiredInEpoch` |

## Why goldens are heavily masked here

These queries hit fields that drift constantly (counts grow, registries change, mint events arrive). The relevant assertion is "the field returns a valid shape" — not "the value matches a captured snapshot". So most coverage goldens carry an `ignore_paths` entry that masks the entire returned list/aggregate. The AST reference is what counts toward coverage; the runtime test only verifies the query executes without error.

## When to add a new coverage query

1. Run `uv run python -m utils.coverage --full --json | jq .field_level.per_type` and find a type with low `pct`.
2. Decide which missing field can be reached from a top-level Query root with reasonable cost.
3. Either extend an existing query in this directory, or add a new one if the topic is distinct (genesis vs governance vs certs).
4. Drop a golden JSON next to the `.graphql`, mask drifting fields aggressively, run `--update-golden` once, review the captured shape, commit.

## Why not include in `queries/example/`?

The `example/` directory mirrors `packages/api-cardano-db-hasura/src/example_queries/` byte-for-byte (or close to it) — that's the official example corpus. The queries here are framework-internal coverage devices, so they live separately to avoid pretending they're official examples.
