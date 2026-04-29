# cardano-graphql Python test framework

Rosetta-inspired, read-only test suite for any GraphQL implementation of the `cardano-graphql` schema. Runs against a live endpoint and verifies correctness via golden snapshots pinned to a fixed block.

**Status**

- Top-level Query coverage: **52 / 52 (100 %)**
- Field-level coverage: **428 / 440 (97.3 %)** across 61 / 61 data types touched
- 59 mainnet goldens + 8 preprod scaffolds + 8 negative + 7 schema (1 integration + 6 unit) + parametrized multi-instance compare
- CI wired (PR gate / nightly / weekly), Locust load harness with baseline gate
- 52 auto-markers (one per Query root) + `cip26` + `cip68`

**Where to start**

| If you want to… | Read |
|---|---|
| Install + run in 5 minutes | `QUICKSTART.md` |
| Understand all test types and what results mean | `TESTING_GUIDE.md` |
| Add a new test or refresh the anchor | `CONTRIBUTING.md` |
| Run perf tests | `performance/README.md` |
| Run multi-instance compare | `multi_instance/README.md` |
| Check schema drift | `schema/README.md` |
| Negative tests | `negative/README.md` |
| Regression tests | `queries/regression/README.md` |
| Coverage queries | `queries/coverage/README.md` |
| Invoke from natural language (Claude skill) | `../.claude/skills/test-cardano-graphql.md` |

**All six phases shipped**

- Phase 1 — sanity, golden functional, negative — `functional/`, `negative/`
- Phase 2 — load / perf with Locust + baseline gate — `performance/`
- Phase 3 — multi-instance comparison (skipped without `--compare-url`) — `multi_instance/`
- Phase 4 — regression tests seeded from GitHub issues — `queries/regression/`
- Phase 5 — schema introspection diff — `schema/`
- Phase 6 — CI workflows (PR gate, nightly, weekly) — `.github/workflows/graphql_tests.yml`

---

## Quick start

```bash
cd tests/
uv sync

# Assumes a GraphQL instance is reachable at the configured URL and is fully synced:
#   { cardanoDbMeta { initialized syncPercentage } }  →  initialized: true, sync >= 99
uv run pytest                       # whole Phase 1 suite
uv run pytest -m sanity -v          # just sanity + readiness
uv run pytest -m golden -v          # golden-file tests
uv run pytest -m negative -v        # error-path tests
uv run pytest -m pr                 # PR-gate subset (fast, must-pass)
uv run pytest -k first20 -v         # by test name
```

Self-contained HTML report (recommended — no external CLI needed):

```bash
uv run python generate_report.py        # full suite
uv run python generate_report.py -- -m pr
open reports/report.html
```

The report includes a per-test table with durations, a live schema-coverage panel (percentage bar + covered/missing field lists), and a query-corpus appendix showing which `.graphql` files touch which root fields.

Allure report (optional, needs the `allure` CLI — `brew install allure`):

```bash
uv run pytest --alluredir=reports/allure-raw
allure serve reports/allure-raw
```

---

## Configuration

| Env var            | Default (pytest.ini)     | Purpose                                              |
|--------------------|---------------------------|------------------------------------------------------|
| `CARDANO_NETWORK`  | `mainnet`                 | Selects the block in `config/networks.yaml`          |
| `GRAPHQL_URL`      | `http://localhost:3100`   | Overrides the URL from `config/networks.yaml`        |
| `CI`               | unset                     | When `true`, blocks `--update-golden` to protect goldens |

`config/networks.yaml` holds the **anchor block** per network plus test addresses, assets, pool ids, and tx hashes. Bump `anchor_block` in a PR when you want to refresh goldens against a newer chain state.

---

## Markers

Two flavors — manual + auto.

**Manual** (declared in `pytest.ini` and applied in test files):

- `pr` — fast, must-pass on every PR.
- `sanity` — `cardanoDbMeta`, chain tip, genesis networkMagic.
- `golden` — golden-file comparison tests.
- `negative` — error-path tests.
- `nightly` — comprehensive, Phase 2+.
- `smoke` — non-blocking data availability.
- `slow` — >5s budget.
- `multi_instance` — cross-URL compare (Phase 3).
- `regression` — issue-reproducer goldens (Phase 4).
- `schema` — schema introspection diff (Phase 5).
- `requires_token_registry` — instance must have TOKEN_REGISTRY_ENABLED.

**Auto** (computed at collection time from each golden's query AST):

- One marker per top-level `Query.<root>` field — all 52 of them: `assets`, `blocks`, `transactions`, `paymentAddresses`, `stakePools`, `epochs`, `delegationVotes`, `committeeMembers`, `votingAnchor`, `tokenMints`, plus every `*_aggregate` variant, etc.
- `cip26` — Asset selecting any of `name / ticker / description / logo / url / decimals / metadataHash` (token-registry metadata).
- `cip68` — Datum selecting `value` or `bytes` (datum-embedded metadata).

```bash
uv run pytest -m assets                # all goldens touching the `assets` root
uv run pytest -m cip26                  # all token-registry tests
uv run pytest -m cip68                  # all datum-metadata tests
uv run pytest -m "assets or stakePools" # union
uv run pytest -m "cip26 and not slow"   # intersection
uv run pytest                           # everything (no `-m all`; bare pytest is "all")
uv run python -m utils.markers          # list all markers + test counts
uv run python -m utils.markers --tests  # also list per-marker test ids
```

---

## Golden-file workflow

Goldens live under `golden/functional/<category>/<name>.json`. Each names its query file (`query_file`), variables, and `ignore_paths` for volatile fields (e.g., `data.blocks[*].forgedAt`). The `expected` block is populated by `--update-golden`.

**Capture / refresh**:

```bash
# Capture all (fresh bootstrap or full refresh)
uv run pytest --update-golden

# Refresh just one
uv run pytest 'functional/test_golden_examples.py::test_golden[blocks__first20Blocks]' --update-golden
```

Always review the git diff of captured goldens before committing — `--update-golden` is a record-then-review loop, not a CI action. Running `--update-golden` with `CI=true` is blocked on purpose.

**Replay**:

```bash
uv run pytest functional/test_golden_examples.py -v
```

**Ignore-path DSL**: dot notation with `[*]` for every list element. Example:

```json
"ignore_paths": [
  "data.blocks[*].forgedAt",
  "data.cardano.tip.hash",
  "data.transactions[*].inputs[*].sourceTxIndex"
]
```

Fields at ignore paths are removed from both expected and actual before diff.

---

## Determinism

Every golden either:

1. Targets a **pinned anchor block** (`networks.yaml:anchor_block`, currently `13228000` on mainnet) — examples: `paymentAddresses(atBlock: N)`, `blocks where number=N`.
2. Queries fully historical state (finalized epoch, genesis parameters, a known fixed tx hash).
3. Masks volatile fields via `ignore_paths` when shape is stable but values change.

If you find a test that breaks on every chain advance, it's missing one of these three controls.

---

## Project layout

```
tests/
├── config/networks.yaml          # anchor block + test data per network
├── client/graphql_client.py      # httpx POST wrapper
├── queries/
│   ├── example/                  # seeded from packages/.../example_queries/
│   ├── fireblocks/               # extracted from fireblocks CSV
│   └── regression/               # Phase 4 — empty today
├── golden/
│   ├── functional/               # golden snapshots (by category)
│   └── negative/                 # reserved for Phase 2+ golden-negative
├── functional/
│   ├── test_sanity.py            # readiness + invariants
│   └── test_golden_examples.py   # parametrized golden runner
├── negative/test_negative.py     # error-path tests
├── utils/
│   ├── query_loader.py           # load .graphql by path
│   ├── normalize.py              # ignore_paths DSL
│   ├── diff.py                   # deep-diff with path reporting
│   └── golden_io.py              # golden load/save/compare
├── performance/                  # Phase 2 — locust harness + baseline gate
├── multi_instance/               # Phase 3 — empty today
├── reports/                      # Allure output (gitignored)
├── conftest.py                   # session fixtures, Allure hooks
├── pytest.ini
├── pyproject.toml                # uv
└── uv.lock
```

---

## Adding a new golden test

1. Save your GraphQL query to `queries/example/<category>/<name>.graphql` (or `queries/fireblocks/` / `queries/regression/`).
2. Create `golden/functional/<category>/<name>.json`:
   ```json
   {
     "test_name": "my_new_test",
     "description": "What this asserts and why it is deterministic.",
     "network": "mainnet",
     "query_file": "queries/example/<category>/<name>.graphql",
     "variables": { "<var>": "<value pinned to anchor or finalized state>" },
     "anchor_block": 13228000,
     "ignore_paths": [],
     "graphql_errors_allowed": false
   }
   ```
3. Capture: `uv run pytest 'functional/test_golden_examples.py::test_golden[<category>__<name>]' --update-golden -v`.
4. Replay: `uv run pytest 'functional/test_golden_examples.py::test_golden[<category>__<name>]' -v`.
5. Review the populated `expected` block in git diff, then commit both files.

If the response contains values that drift (timestamps, current tip, `syncPercentage`, asset registry metadata), add paths to `ignore_paths` before capturing.

---

## Phase roadmap

| Phase | Scope                                         | Status                                       |
|-------|-----------------------------------------------|----------------------------------------------|
| 1     | Scaffolding + sanity + goldens + negative     | shipped                                      |
| 2     | Load / performance (Locust, p50/p95/p99)      | shipped — see `performance/README.md`        |
| 3     | Multi-instance compare (same query, two URLs) | shipped — `uv run pytest multi_instance/ --compare-url=http://other:3100` |
| 4     | Regression tests seeded from GitHub issues    | shipped — 4 seeded (see `queries/regression/README.md`) |
| 5     | Schema introspection diff (graphql-core)      | shipped — `uv run pytest schema/`            |
| 6     | CI workflows: PR gate, nightly, weekly        | shipped — `.github/workflows/graphql_tests.yml` |

### Coverage

| Metric | Value |
|---|---|
| Top-level `Query` fields covered | **52 / 52 (100%)** |
| Mainnet goldens captured | **59** |
| Preprod goldens scaffolded (expected: null) | **8** |
| Negative tests | **8** |
| Offline schema-diff unit tests | **6** |
| Multi-instance parametrized cases | **59** (skip without `--compare-url`) |
| Query `.graphql` files | **~40** |

Run `uv run python -m utils.coverage` to regenerate the coverage report. The tool parses every query with `graphql-core`, extracts top-level `Query` fields referenced, and diffs against the committed schema snapshot. It also shows up as a panel in `generate_report.py`'s HTML output.

### Address buckets

Phase 1 goldens and Phase 2 perf tasks split address queries into three buckets:

| Bucket   | Representative mainnet address                                             | Typical cost                         |
|----------|----------------------------------------------------------------------------|--------------------------------------|
| `light`  | retail wallet, seen in 1-3 txs at anchor                                   | few UTxOs → fastest response         |
| `medium` | active wallet, recurring input/output at anchor                            | tens of UTxOs → moderate response    |
| `heavy`  | DEX/script contract (SundaeSwap v3 pool at anchor)                         | many UTxOs → slow; excluded in `light` perf profile |

Values pinned in `config/networks.yaml` under `test_addresses.{light,medium,heavy}`. Each bucket gets its own golden (`golden/functional/payment_addresses/paymentAddressSummary_{light,medium,heavy}.json`) and its own Locust task, so tail latency on heavy addresses is isolated in perf metrics.

See `/Users/kartikiyer/.claude/plans/look-at-task-md-and-purrfect-fountain.md` for the full plan.

---

## Troubleshooting

- **Readiness fails**: instance is not synced (`syncPercentage < 99`) or tip is below the anchor. Wait for sync, or lower `anchor_block` in `networks.yaml` (will invalidate all anchored goldens).
- **Timeouts on UTxO / paymentAddressSummary**: heavy queries. Bump `timeout_s` in `networks.yaml` — current default is 180s.
- **Many goldens drift after merging a `packages/api-cardano-db-hasura/schema.graphql` change**: expected — re-run `--update-golden` and review.
- **CI is trying to run `--update-golden` and failing**: that is intentional. Goldens are captured locally and reviewed in PR; CI only replays.
