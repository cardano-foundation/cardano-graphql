# Phase 2 — Load / performance tests

Locust-based harness that drives representative GraphQL queries against a configured endpoint, captures per-query latency and error rate, and gates results against task.md baselines (**p95 < 500 ms, error rate < 1 %**).

Reuses Phase 1's `config/networks.yaml` (URL + anchor block + test data) and `queries/**/*.graphql` (single source of truth for query text). No code is duplicated — the locustfile imports `performance/_config.py` which calls into the same YAML and query files.

---

## Files

```
performance/
├── _config.py           # tiny loader: networks.yaml → dict, query_file → str
├── locustfile.py        # Locust HttpUser with weighted @tasks, one per query
├── run_load.py          # CLI wrapper around `locust` with named profiles
├── check_baselines.py   # parses Locust stats CSV, enforces p95/err-rate caps
└── README.md            # this file
```

---

## Profiles

`run_load.py --profile=<name>` picks preset users / spawn rate / duration:

| Profile     | Users | Spawn rate | Duration | Intent                                  |
|-------------|-------|------------|----------|-----------------------------------------|
| `smoke`     | 5     | 1/s        | 60s      | CI sanity — verifies plumbing, not perf |
| `soak`      | 20    | 2/s        | 30m      | Steady-state stability                  |
| `stability` | 50    | 5/s        | 5m       | Capacity / percentile probe             |

All three can be overridden per-run with `--users`, `--spawn-rate`, `--run-time`.

### Query-mix profiles (orthogonal to the above)

`--load-profile=full|light` (default `full`):

- `full` — includes heavy tasks (`paymentAddressSummary`, `transactionsInBlock`), weighted low.
- `light` — skips them. Use this when an endpoint is new, still catching up, or known-slow for those queries.

---

## Run it

```bash
cd tests/
uv sync                                           # locust pulled in automatically

# Smoke — 60s, 5 users, light mix
uv run python performance/run_load.py --profile=smoke --load-profile=light

# Soak — 30 minutes, 20 users
uv run python performance/run_load.py --profile=soak

# Stability — 5 min, 50 users, also emit an HTML report
uv run python performance/run_load.py --profile=stability \
  --html=reports/perf-stability.html

# Free-form
uv run python performance/run_load.py --users=100 --spawn-rate=10 --run-time=2m
```

Stats land in `reports/perf-<profile>-<timestamp>_*.csv` plus a `_stats_history.csv` for timeseries.

---

## Baseline gate

After a run, pass the stats CSV (or just the `--csv` prefix) to the baseline checker:

```bash
uv run python performance/check_baselines.py \
  reports/perf-smoke-20260423T213232_stats.csv

# Or pass the prefix — the _stats.csv suffix is added automatically:
uv run python performance/check_baselines.py reports/perf-smoke-20260423T213232

# Override thresholds (defaults: p95=500 ms, err-rate=1%):
uv run python performance/check_baselines.py reports/<prefix> --p95=800 --err-rate=0.5

# Skip the Aggregated roll-up row:
uv run python performance/check_baselines.py reports/<prefix> --ignore-aggregated
```

Exit codes:

| Code | Meaning                                              |
|------|------------------------------------------------------|
| 0    | All endpoints within baseline                        |
| 1    | At least one endpoint breached p95 or error-rate cap |
| 2    | Bad input (missing CSV, parse error)                 |

Intended for wiring into CI as a hard gate after the load run.

---

## Query corpus

Drawn from Phase 1's `queries/` tree, weighted to approximate realistic traffic:

| Query                               | Weight | Bucket | File                                                      |
|-------------------------------------|--------|--------|-----------------------------------------------------------|
| `cardanoTip`                        | 25     | –      | `queries/fireblocks/cardanoTip.graphql`                   |
| `blockByNumber`                     | 20     | –      | `queries/fireblocks/blockByNumber.graphql`                |
| `cardanoDbSyncProgress`             | 15     | –      | `queries/example/meta/cardanoDbSyncProgress.graphql`      |
| `stakePoolById`                     | 10     | –      | `queries/example/stake_pools/stakePoolById.graphql`       |
| `transactionByHash`                 | 10     | –      | `queries/fireblocks/transactionByHash.graphql`            |
| `paymentAddressSummary_light`       | 8      | light  | `queries/fireblocks/paymentAddressSummary.graphql`        |
| `selectGreatGrandchildBlock`        | 5      | –      | `queries/example/blocks/selectGreatGrandchildBlock.graphql` |
| `epochDetailsByNumber`              | 5      | –      | `queries/example/epochs/epochDetailsByNumber.graphql`     |
| `utxoSetForAddresses_light`         | 4      | light  | `queries/fireblocks/utxoSetForAddresses.graphql`          |
| `assets_first3`                     | 3      | –      | `queries/example/assets/assets.graphql`                   |
| `paymentAddressSummary_medium`      | 3      | medium | `queries/fireblocks/paymentAddressSummary.graphql`        |
| `transactionsInBlock`               | 1      | –      | `queries/fireblocks/transactionsInBlock.graphql`          |
| `paymentAddressSummary_heavy`       | 1      | heavy  | `queries/fireblocks/paymentAddressSummary.graphql`        |
| `utxoSetForAddresses_heavy`         | 1      | heavy  | `queries/fireblocks/utxoSetForAddresses.graphql`          |

**Bucket tasks** each appear as a distinct endpoint in Locust stats and in `check_baselines.py` — tail latency on heavy addresses is isolated from retail-wallet latency instead of smeared across `Aggregated`. `--load-profile=light` drops the medium and heavy bucket tasks entirely.

Variables pinned to values from `config/networks.yaml`: `anchor_block`, `test_addresses`, `test_transactions_at_anchor`, `test_stake_pools`, `test_asset_ids`, `test_epochs.finalized`. So every run exercises the same rows on the server — latency differences reflect server state and code, not a moving target.

### Adding / removing a task

Each `@task(<weight>)` method in `locustfile.py` loads a `.graphql` file and issues one POST. Weights are relative: doubling a weight doubles the fraction of traffic that task gets. To add a query:

1. Make sure the `.graphql` file is under `tests/queries/`.
2. Add values it needs to `config/networks.yaml` (under `test_*`).
3. Add a `@task(N)` method in `locustfile.py`.

---

## Failure semantics

Every task uses `catch_response=True` and marks the Locust request failed when:

- HTTP status != 200, **or**
- the response body contains a non-empty `errors` array.

The error message is attached to the failure, so `reports/*_failures.csv` records what went wrong. This matters because the cardano-graphql server returns HTTP 200 with an `errors` block for runtime resolver failures — naive status-code checks would miss these.

---

## Troubleshooting

- **Readiness / sync not checked here**: Phase 2 does not poll `cardanoDbMeta` before running. If the endpoint is still syncing, expect failures, not slow-but-correct responses. Run `uv run pytest functional/test_sanity.py -v` first to confirm readiness.
- **A single slow endpoint fails the gate**: investigate the server side (missing index? cold cache?). As a short-term workaround you can raise `--p95` locally or re-run with `--load-profile=light` to exclude heavy queries.
- **Locust web UI**: drop `--headless` from `run_load.py`'s command list (or edit the wrapper) to get Locust's browser dashboard on `:8089`.
- **Stats CSV has no percentile columns**: the `95%` column requires `--csv-full-history` (already set by the wrapper) — if you invoke locust directly, keep that flag.

---

## What's next

- Phase 3 — multi-instance comparison (same query, two URLs, diff + latency compare).
- Phase 4 — regression goldens seeded from GitHub issues.
- Phase 5 — schema introspection diff.
- Phase 6 — CI workflows: PR gate, nightly, weekly.
