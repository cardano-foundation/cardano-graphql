# Testing guide

Skim-friendly reference for what tests exist, what they cover, and what results mean.

For install/run in 5 min → `QUICKSTART.md`.
For adding a test → `CONTRIBUTING.md`.

---

## Types of testing — at a glance

| Type | Marker | What it does | Lives in |
|---|---|---|---|
| **Sanity** | `sanity` | Confirms the endpoint is healthy, synced, and the anchor block is reachable | `functional/test_sanity.py` |
| **Golden functional** | `golden` | Runs each `.graphql` against the live endpoint, diffs response against a stored snapshot | `functional/test_golden_examples.py` + `golden/functional/**/*.json` |
| **Negative** | `negative` | Sends bad input, asserts the server returns errors correctly | `negative/test_negative.py` |
| **Multi-instance compare** | `multi_instance` | Same query against two endpoints, asserts equivalent responses | `multi_instance/test_compare.py` |
| **Schema drift** | `schema` | Diffs live introspection against a committed snapshot | `schema/test_schema_snapshot.py` |
| **Schema diff logic (offline)** | `schema` | Unit tests for the diff function itself, no endpoint | `schema/test_schema_diff_logic.py` |
| **Regression (issue-seeded)** | `regression` (auto) | Reproduce closed bugs from the GitHub tracker | `golden/functional/regression/**/*.json` |
| **Load / performance** | (none — separate harness) | Locust workload; checks p95 < 500 ms, error rate < 1 % | `performance/locustfile.py` + `run_load.py` + `check_baselines.py` |

---

## What each type covers

### Sanity (4 tests, ~1 s total)
- `cardanoDbMeta { initialized syncPercentage }` → must be `true` and ≥ 99 %.
- `cardano.tip.number` → must be ≥ the configured anchor block.
- `genesis.shelley.networkMagic` → must equal the value in `config/networks.yaml`.
- The anchor block itself is fetchable.

### Golden functional (59 mainnet + 8 preprod scaffolds)
- Covers **52 / 52 top-level Query roots** (100 %).
- Covers **428 / 440 data-type fields** (97.3 %) across 61 types.
- Each test = one `.graphql` query + one JSON config + one captured `expected` block.
- All results are pinned to anchor block `13228000` on mainnet (or to immutable historical state).

### Negative (8 tests, ~3 s total)
- Missing required variable → server flags it.
- Wrong variable type → coercion error.
- Unknown field → validation error.
- Malformed GraphQL syntax → parse error.
- Invalid filter operator (`_gtLte`) → bool-exp validation error.
- Out-of-range block number → empty result, no error (graceful).
- Non-hex value for `Hash32Hex!` scalar → scalar error.
- Negative limit → either error or graceful empty (both accepted).

### Multi-instance compare (parametrized, skipped without `--compare-url`)
- Every Phase 1 golden runs against the primary AND a secondary URL.
- Diffs the two normalized responses.
- Fails if any non-masked path diverges.
- Use case: validating that a fork or alternate implementation returns identical data.

### Schema drift (1 integration + 6 unit tests)
- Integration: introspection result vs `schema/snapshot.json` → fails if anything changed.
- Unit: catches add/remove field, nullability flip, enum-value removal, kind change, input-field changes.

### Regression (4 issue-seeded, more on demand)
| Issue | What it covers |
|---|---|
| `#951` | Tx metadata with nullable `value` must serialize |
| `#953` | Token quantity null must not crash |
| `#978` (fix `#980`) | Token-registry metadata with large strings (`ERR_STRING_TOO_LONG`) |
| `#979` | Backfilled assets must be queryable with `tokenMints` |

Add more via `tests/queries/regression/fetch_issues.py`.

### Load / performance
- 10 Locust tasks with weighted distribution (cheap queries dominate, heavy address queries rare).
- 3 profiles: `smoke` (5 users / 60 s), `soak` (20 / 30 m), `stability` (50 / 5 m).
- 3 address buckets: `light`, `medium`, `heavy` (separately tracked p95).
- Baseline gate: p95 < 500 ms, error rate < 1 % per endpoint. Configurable via flags.

---

## What the results mean

### Pass / fail outcomes

| Pytest output | Meaning |
|---|---|
| `passed` | Query returned, response matched golden after `ignore_paths` masking |
| `failed: Golden diff for <name>:` | Captured snapshot disagrees with current response — see diff lines below |
| `failed: Unexpected GraphQL errors:` | Server returned `errors[]` for a query not flagged `graphql_errors_allowed: true` |
| `failed: httpx.ReadTimeout` | Server didn't respond within `timeout_s` (default 300 s) — heavy query or sick endpoint |
| `failed: <fixture> failed readiness` | Sanity gate failed — the endpoint isn't synced or unreachable |
| `skipped: golden network=preprod, running network=mainnet` | Network filter — golden only runs on its declared network |
| `skipped: --compare-url not provided` | Multi-instance test, no secondary URL given |

### Reading a golden diff

```
- data.blocks[0].hash: expected 'be06c81f4...', got 'aa11bb22cc...'
- data.epochs[0].adaPots.fees: expected '37112078113', got '41534723039'
- data.transactions: list length mismatch — expected 23, got 24
```

Each line is one path:
- **scalar diff** → expected vs actual values
- **list length mismatch** → array grew or shrank
- **type mismatch** → field type changed (real schema regression)
- **missing in actual** → server stopped returning the field
- **unexpected in actual** → new field appeared

### What to do when a golden fails

1. Is it a timestamp / count / unordered list element? → add path to `ignore_paths`, replay (no `--update-golden` needed).
2. Is it a hash / id / block number / asset id mismatch? → real change, investigate the server.
3. Did many goldens fail at once? → run `uv run pytest schema/` first. Schema change usually shows up there too.
4. Did one heavy query time out? → bump `timeout_s` in `config/networks.yaml` for that network.

### Performance baseline outputs

```
[OK]     blockByNumber             n=18  p95=  140ms  fail=0 (0.00%)
[BREACH] stakePoolById             n=12  p95=31000ms  fail=0 (0.00%)
[BREACH] paymentAddressSummary_heavy n=5  p95=132000ms fail=0 (0.00%)
```

- `[OK]` → within ceiling (default p95 < 500 ms).
- `[BREACH]` → exceeded ceiling. Real signal — usually a missing index on the server side.
- `n=` → request count for this endpoint during the run.
- `fail=0 (0.00%)` → request error rate (HTTP error or `errors[]` non-empty).

### Schema drift outputs

```
- Block.forgedAt removed
- Block.size added
- Block.number type: Int -> Int!
- OrderDir.DESC (enum) removed
```

Each line = one breaking change. To accept (intentional): run `uv run pytest schema/ --update-schema -v` and commit `snapshot.json`.

---

## How to run a specific slice

| Goal | Command |
|---|---|
| Everything | `uv run pytest` |
| Just the PR-gate fast subset | `uv run pytest -m pr` |
| Just sanity | `uv run pytest -m sanity` |
| Just goldens | `uv run pytest -m golden` |
| Just negative | `uv run pytest -m negative` |
| Just schema drift | `uv run pytest schema/` |
| All tests touching the `assets` Query root | `uv run pytest -m assets` |
| All tests touching any specific root (52 available) | `uv run pytest -m <root_name>` |
| Token-registry metadata coverage (CIP-26) | `uv run pytest -m cip26` |
| Datum-embedded metadata (CIP-68) | `uv run pytest -m cip68` |
| Union of two markers | `uv run pytest -m "assets or transactions"` |
| Intersection / exclusion | `uv run pytest -m "cip26 and not slow"` |
| Compare two endpoints | `uv run pytest multi_instance/ --compare-url=http://other:3100` |
| Self-contained HTML report | `uv run python generate_report.py && open reports/report.html` |
| List all available markers | `uv run python -m utils.markers` |
| Print coverage report | `uv run python -m utils.coverage --full` |
| Smoke load test | `uv run python performance/run_load.py --profile=smoke` |
| Capture / refresh goldens | `uv run pytest --update-golden` (forbidden when `CI=true`) |
| Refresh schema snapshot | `uv run pytest schema/ --update-schema` |

---

## Markers reference

### Manual markers (declared in `pytest.ini`, applied in test files)

| Marker | Meaning |
|---|---|
| `pr` | Fast must-pass on every PR (~30 tests, < 60 s) |
| `sanity` | Endpoint health, tip, genesis, anchor block |
| `golden` | Golden-file comparison tests |
| `negative` | Error-path tests |
| `nightly` | Comprehensive scheduled run |
| `weekly` | Adds perf smoke + baseline gate |
| `smoke` | Non-blocking data availability |
| `slow` | > 5 s single-test budget |
| `multi_instance` | Cross-URL compare |
| `regression` | Issue-reproducer goldens |
| `schema` | Schema introspection diff |
| `requires_token_registry` | Needs `TOKEN_REGISTRY_ENABLED` on the instance |

### Auto markers (computed at collection time from query AST)

- One per top-level `Query.<root>` field. All 52 of them — `assets`, `blocks`, `transactions`, `paymentAddresses`, `stakePools`, `epochs`, `delegationVotes`, `committeeMembers`, `votingAnchor`, `tokenMints`, plus every `*_aggregate` variant.
- `cip26` — query selects any of `Asset.{name,ticker,description,logo,url,decimals,metadataHash}`.
- `cip68` — query selects `Datum.value` or `Datum.bytes`.

To enumerate every marker + which tests carry it:
```bash
uv run python -m utils.markers --tests
```

---

## Coverage measurements

```bash
uv run python -m utils.coverage --full
```

Produces two numbers:

- **Top-level Query coverage** — fraction of `Query.<field>` referenced by some query. Currently **52 / 52 (100 %)**.
- **Field-level coverage** — fraction of every Object/Interface field across the schema referenced by some query, after excluding Hasura infrastructure. Currently **428 / 440 (97.3 %)** across 61 / 61 data types touched.

Field-level details show:
- Per-type breakdown sorted by coverage %.
- Untouched types (currently 0 — the dozen missing fields are scattered across partially-covered types).
- Hover any row in the HTML report (`generate_report.py`) for the missing field list.

---

## Test data — how it's pinned

All `golden/**/*.json` configs reference values from `tests/config/networks.yaml`:

| Key | Value (mainnet) | Used by |
|---|---|---|
| `anchor_block` | `13228000` | Block / tx / address-at-block queries |
| `test_addresses.light` | `addr1qy2jwtle4le…` | Light bucket — small wallet |
| `test_addresses.medium` | `addr1q9h4f2vhh5…` | Medium bucket — active wallet |
| `test_addresses.heavy` | `addr1z8p79rpkcdz…` | Heavy bucket — DEX/contract script |
| `test_asset_ids` | three stable mainnet assets | Asset queries, token mints |
| `test_stake_pools[0].id` | IOG pool | Pool queries |
| `test_transactions_at_anchor` | two tx hashes from the anchor block | Tx queries |
| `test_epochs.finalized` | `620` | Epoch queries (immutable past epoch) |

Goldens reference these directly. Bumping anchor → see `CONTRIBUTING.md` for the refresh procedure.

Preprod has its own block (`4500000`) and a separate set of test values pulled from Rosetta's `network_test_data.yaml`. Preprod-tagged goldens auto-skip when running mainnet.

---

## CI integration

`.github/workflows/graphql_tests.yml` defines four jobs:

| Job | Trigger | What it does |
|---|---|---|
| `pr-gate` | Every PR touching `tests/`, `schema.graphql`, or example queries | `pytest -m pr`, uploads HTML report |
| `nightly` | 02:15 UTC daily | Full functional + negative + schema, uploads HTML |
| `weekly` | 03:30 UTC Sunday | Full suite + perf smoke + baseline gate, uploads HTML + perf CSVs |
| `manual` | `workflow_dispatch` | User-triggered with optional URL override + `run_perf` toggle |

All jobs export `CI=true`, which the framework honors to block `--update-golden` and `--update-schema`.

---

## Files at a glance

```
tests/
├── README.md                       # framework overview
├── QUICKSTART.md                   # 5-minute install + run
├── TESTING_GUIDE.md                # this file
├── CONTRIBUTING.md                 # how to add tests + masking guide
│
├── conftest.py                     # session fixtures + auto-marker hook
├── pytest.ini                      # markers, default env, addopts
├── pyproject.toml                  # uv-managed deps
│
├── client/graphql_client.py        # httpx wrapper with response recording
├── config/networks.yaml            # anchor block + test data per network
│
├── functional/                     # sanity + golden-replay tests
├── negative/                       # error-path tests   (negative/README.md)
├── multi_instance/                 # cross-URL compare  (multi_instance/README.md)
├── schema/                         # introspection diff (schema/README.md)
├── performance/                    # Locust + baseline gate (performance/README.md)
│
├── queries/
│   ├── example/                    # mirrors packages/.../example_queries/
│   ├── fireblocks/                 # extracted from the fireblocks CSV
│   ├── coverage/                   # framework-internal compound queries
│   └── regression/                 # issue-reproducer queries (regression/README.md)
├── golden/functional/<category>/   # captured `expected` blocks per query
│
├── utils/
│   ├── coverage.py                 # schema coverage tool
│   ├── markers.py                  # `python -m utils.markers` lister
│   ├── diff.py                     # deep diff with path-based ignores
│   ├── normalize.py                # ignore_paths DSL
│   ├── golden_io.py                # load/save/compare golden files
│   ├── query_loader.py             # .graphql loader
│   └── schema_snapshot.py          # canonicalize introspection + diff
│
├── generate_report.py              # self-contained HTML report builder
└── reports/                        # gitignored — HTML reports, perf CSVs, allure
```
