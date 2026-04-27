# Contributing to the cardano-graphql test framework

How to add a new test, refresh an anchor, debug a failure, and decide what to mask.

If you only want to *run* tests, see `QUICKSTART.md`. If you want to know *what* is tested, see `README.md`.

---

## How to add a new golden test

Five minutes if the query already exists, ~15 minutes if you need to write it.

### 1. Pick a query

Three places queries live:

- `queries/example/<category>/<name>.graphql` — copies of `packages/api-cardano-db-hasura/src/example_queries/`.
- `queries/fireblocks/<name>.graphql` — extracted from `fireblocks-query-analysis - Sheet1.csv`.
- `queries/coverage/<name>.graphql` — framework-internal compound queries that push schema coverage.

If your query already exists in upstream `example_queries/`, copy it into `queries/example/<category>/`. Otherwise put it in `queries/coverage/` (or wherever fits the source of truth).

### 2. Write the golden config

Drop a JSON next to the matching path under `golden/functional/<category>/`:

```json
{
  "test_name": "blockByNumber_anchor",
  "description": "<one line: what this asserts and why it's deterministic>",
  "network": "mainnet",
  "query_file": "queries/example/blocks/blockByNumber.graphql",
  "variables": {
    "number": 13228000
  },
  "anchor_block": 13228000,
  "ignore_paths": [],
  "graphql_errors_allowed": false
}
```

Fields:

- `test_name`: free text, shown in pytest output.
- `description`: one line. Required. Future-you needs context when this fails in 6 months.
- `network`: `mainnet` or `preprod`. Goldens whose network doesn't match the active `CARDANO_NETWORK` are auto-skipped.
- `query_file`: relative to `tests/`. Points at the `.graphql` file above.
- `variables`: GraphQL variables. Pin to values from `config/networks.yaml` (anchor block, test addresses, test asset ids, test pool ids, finalized epoch) or to literal historical values.
- `anchor_block`: the block height the test depends on, or `null` if the query is genuinely block-independent (e.g., `genesis`, `cardano.tip` with masking). Documents the dependency.
- `ignore_paths`: list of dot-paths to mask before diffing. See "Masking decisions" below.
- `graphql_errors_allowed`: `false` by default. Only set `true` for known backend-error tests where the query is intentionally a regression test for a server bug.
- `bucket` (optional): `light` / `medium` / `heavy` for address-bucketed tests.

### 3. Capture the expected response

```bash
uv run pytest 'functional/test_golden_examples.py::test_golden[<category>__<test_name>]' --update-golden -v
```

This captures the live response into the golden's `expected` block. Forbidden when `CI=true`.

### 4. Review, replay, commit

```bash
git diff tests/golden/functional/<category>/<test_name>.json     # review captured `expected`
uv run pytest 'functional/test_golden_examples.py::test_golden[<category>__<test_name>]' -v
git add tests/queries/.../<name>.graphql tests/golden/functional/.../<test_name>.json
git commit -m "test: golden for <what it covers>"
```

### 5. Verify markers and coverage

```bash
uv run python -m utils.markers --tests | grep <test_name>
uv run python -m utils.coverage --full
```

Auto-markers are derived from your query's AST — no manual marker tagging needed.

---

## Masking decisions: what goes in `ignore_paths`?

Mask fields that **change between runs for reasons unrelated to data correctness**. Don't mask fields that *should* match — that defeats the test.

| Pattern | Mask? |
|---|---|
| Timestamps (`forgedAt`, `includedAt`, `startedAt`, `lastBlockTime`) | yes — drift with chain time, no functional relevance to the query |
| Aggregate counts that grow with the chain (`*_aggregate.aggregate.count`, ADA `circulating`/`total`) | yes if the query has no time-bounding `where` clause |
| Token-registry metadata (`Asset.{name,ticker,description,logo,url,decimals,metadataHash}`) | yes when querying *any* asset across the registry (registry can refresh independently of chain state) |
| Whole list with no `order_by` clause (Hasura returns implementation-defined order) | yes — mask the entire list path |
| `tokenMints` array on an asset / `utxos` array on an address | yes — both grow as the asset is minted / address transacts |
| Specific asset id, block hash, transaction hash | NO — these are the assertion. If they drift you've got a deterministic-pinning bug. |
| `paymentAddresses(addresses:[...]).summary(atBlock: N).assetBalances` | NO — `atBlock` makes this immutable. If it drifts, your anchor or test address changed. |

### When `data.<list>` masks the whole array

```json
"ignore_paths": [
  "data.scripts"
]
```

This drops `data.scripts` from both expected and actual before diffing. Use when the query has no `order_by` and the test goal is "the query executes correctly", not "the rows match exactly". Common for `scripts(limit: N)`, `redeemers(limit: N)`, `delegationVotes(limit: N)` etc.

### When you need per-element field ignores

```json
"ignore_paths": [
  "data.epochs[*].startedAt",
  "data.epochs[*].lastBlockTime"
]
```

The `[*]` wildcard applies the path to every element of the surrounding list. So `data.epochs[*].startedAt` strips `startedAt` from every element of `data.epochs`. The non-volatile sibling fields (`adaPots`, `blocksCount`, `transactionsCount`) are still asserted strictly.

---

## How to refresh the anchor block

Goldens go stale only when you intentionally bump the anchor:

```bash
# 1. Pick a new anchor — must be ≥24h old (so it's beyond rollback risk) and ≤ current sync head
new_anchor=13500000

# 2. Edit tests/config/networks.yaml — bump `mainnet.anchor_block` and `mainnet.test_blocks.anchor`

# 3. Find a transaction in the new anchor block and bump test_transactions_at_anchor
curl -sS -X POST http://localhost:3100/graphql -H "Content-Type: application/json" \
  -d '{"query":"{ transactions(where:{block:{number:{_eq: '"$new_anchor"'}}}, order_by:{hash:asc}, limit:2){hash}}"}'

# 4. Recapture every anchor-dependent golden
uv run pytest -m golden --update-golden

# 5. Diff, review carefully, commit on a dedicated branch
git diff tests/golden/
```

The anchor exists so tests are deterministic; bumping it is a deliberate decision, never automatic.

---

## How to debug a failing test

1. **Run the single test with `-v` and look at the diff:**
   ```bash
   uv run pytest 'functional/test_golden_examples.py::test_golden[<id>]' -v
   ```
   The framework prints a path-by-path diff (`utils/diff.py`).

2. **Was it data drift or a real regression?**
   - If the diff is on a timestamp / count / unordered-list element → the field needs `ignore_paths`. Real value didn't change; test was over-strict.
   - If the diff is on a hash / id / block number / asset id → real change. Either the chain rolled back (very rare), the anchor was bumped, or the server is genuinely broken.

3. **Run the query yourself:**
   ```bash
   curl -sS -X POST $GRAPHQL_URL/graphql -H 'Content-Type: application/json' \
     -d "$(jq -nc --argjson v "$(jq .variables tests/golden/functional/<...>.json)" --arg q "$(cat tests/queries/...)" '{query:$q, variables:$v}')"
   ```
   Compare against `expected` in the golden.

4. **Allure attaches request + response + diff** for every test. Use `uv run python generate_report.py` and click into the failure for full context.

5. **Schema drift?** If lots of goldens fail simultaneously with shape mismatches, run `uv run pytest schema/` first — the schema may have changed from under you.

---

## Adding a new sanity / negative / schema test

These are not parametrized, so just write a `test_*` function in the matching directory. Use `pytest.mark.<name>` for filtering. Examples are in the existing files.

---

## Updating the framework itself

- Code changes: keep the layered architecture intact (client → query loader → normalize → diff → io). Avoid coupling test code directly to `httpx` or to JSON-shape assumptions outside `utils/`.
- Marker changes: registered dynamically in `pytest_configure` (root markers) or hard-coded in `pytest.ini` (manual markers). Don't add markers in test files without registering them.
- New utilities: drop in `utils/`, exposing a clean function API. Keep modules small and single-purpose.
- New CLI tools: top-level scripts (`generate_report.py`, `performance/run_load.py`, etc.) — make them runnable as `uv run python <path>` with `argparse`.

---

## What we deliberately don't do

- **Re-derive expected values dynamically.** Goldens are static snapshots reviewed in PR. We don't compute "expected hash should be hash-of-block-N at runtime" — that hides bugs where both expected and actual would shift together.
- **Mock the GraphQL server in tests.** Same lesson: mocked tests passing while production fails. We always run against a real (local or remote) endpoint.
- **Auto-update goldens in CI.** `--update-golden` is forbidden when `CI=true`. Goldens are a deliberate artifact; only humans update them.
- **Test mutations.** `submitTransaction` exists in the schema but is excluded by design — this is a read-side framework. Transaction construction/signing is Rosetta's job, not ours.

---

## Useful commands cheat-sheet

```bash
# Coverage at top level + field-level
uv run python -m utils.coverage --full

# Marker enumeration
uv run python -m utils.markers --tests

# Self-contained HTML report with coverage panel
uv run python generate_report.py
open reports/report.html

# Per-root or CIP filters
uv run pytest -m assets -v
uv run pytest -m "cip26 or cip68"
uv run pytest -m "transactions and not slow"

# Update one golden
uv run pytest 'functional/test_golden_examples.py::test_golden[<id>]' --update-golden -v

# Schema drift check
uv run pytest schema/ -v

# Refresh schema snapshot (intentional schema change)
uv run pytest schema/test_schema_snapshot.py --update-schema -v

# Compare two endpoints
uv run pytest multi_instance/ --compare-url=http://other:3100 -v

# Load test smoke
uv run python performance/run_load.py --profile=smoke --load-profile=light
uv run python performance/check_baselines.py reports/perf-smoke-*_stats.csv
```
