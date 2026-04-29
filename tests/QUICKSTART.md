# cardano-graphql tests — Quickstart

From zero to a green test run in under five minutes. Run everything from the `tests/` directory.

---

## 1. Prerequisites

- **Python 3.12+** (the framework runs on 3.12 or 3.14; either is fine).
- **uv** — the Python package manager. Install once:
  ```bash
  curl -LsSf https://astral.sh/uv/install.sh | sh
  ```
- **A reachable cardano-graphql endpoint** at `http://localhost:3100` (or pass `GRAPHQL_URL=...`). If your instance is remote behind SSH:
  ```bash
  ssh -f -N -L 3100:localhost:3100 mainnet
  ```

## 2. Install

```bash
cd tests/
uv sync
```

`uv sync` reads `pyproject.toml` + `uv.lock` and installs everything into a local `.venv/`. Takes ~20s the first time.

## 3. Smoke check

```bash
uv run pytest -m sanity -v
```

This confirms the endpoint is reachable, `cardanoDbMeta.initialized == true`, the chain tip is past the configured anchor block, the network magic matches the config, and the anchor block itself is fetchable. If this passes, everything downstream will work.

## 4. Full run

```bash
uv run pytest -v
```

Runs sanity + 56 goldens + 8 negative tests + schema snapshot check. Expected time: 8-15 minutes on a healthy endpoint. 8 preprod-tagged goldens skip unless you set `CARDANO_NETWORK=preprod`.

## 5. Self-contained HTML report

```bash
uv run python generate_report.py
open reports/report.html
```

Produces a single standalone HTML file at `reports/report.html` with:

- **Header banner** — date, run time, network, endpoint.
- **Per-test table** — each test id, status (PASS/FAIL/SKIP), duration, short traceback on failure.
- **Schema coverage panel** — percentage of top-level `Query` fields exercised, covered list, missing list.
- **Query corpus appendix** — every `.graphql` file and which root fields it touches.

To run a subset:

```bash
uv run python generate_report.py -- -m pr       # PR-gate subset
uv run python generate_report.py -- -k golden   # only golden tests
```

## 6. Load / performance

```bash
uv run python performance/run_load.py --profile=smoke
uv run python performance/check_baselines.py reports/perf-smoke-*_stats.csv
```

Profiles: `smoke` (5 users, 60s), `soak` (20 users, 30m), `stability` (50 users, 5m). The baseline checker enforces **p95 < 500 ms** and **error rate < 1 %** per endpoint.

## 7. Schema drift check

```bash
uv run pytest schema/ -v
```

Fetches introspection, canonicalizes, diffs against `schema/snapshot.json`. Any breaking change (removed fields, nullability flips, enum-value removal) fails loud. When a schema change is intentional:

```bash
uv run pytest schema/ --update-schema -v
git diff tests/schema/snapshot.json   # review
```

## 8. Compare two instances

```bash
ssh -f -N -L 3101:localhost:3100 my-other-instance   # secondary tunnel
uv run pytest multi_instance/ --compare-url=http://localhost:3101 -v
```

Every golden is replayed against both the primary (`GRAPHQL_URL`) and the secondary (`--compare-url`); any non-masked divergence fails the test.

## 9. Refresh goldens

```bash
uv run pytest --update-golden -v
```

Captures the current live response into every golden's `expected` block. **Never run with `CI=true`** — the framework refuses it.

## 10. Coverage

```bash
uv run python -m utils.coverage
```

Parses every `queries/**/*.graphql`, cross-references `schema/snapshot.json`, prints coverage %. Current coverage: **52/52 top-level Query fields (100%)**.

---

## Environment variables

| Var | Default | Meaning |
|---|---|---|
| `CARDANO_NETWORK` | `mainnet` | Picks the block under `config/networks.yaml`. |
| `GRAPHQL_URL` | from config | Overrides the URL for the primary instance. |
| `CI` | unset | When `true`, blocks `--update-golden` and `--update-schema`. |

## Markers (for `-m` filters)

`sanity`, `golden`, `negative`, `pr`, `nightly`, `smoke`, `slow`, `multi_instance`, `schema`, `regression`, `requires_token_registry`.

## Troubleshooting

- **Sanity fails with readiness timeout**: the endpoint is not synced (`syncPercentage < 99`). Wait, or bump `timeout_s` in `config/networks.yaml`.
- **Heavy queries time out at 180-300s**: expected on deployments without efficient indexes for address-filtered queries. Use `--load-profile=light` in perf, or swap the `medium`/`heavy` test addresses for a smaller set.
- **SSH tunnel hung**: stale connections from earlier runs can jam `localhost:3100`. Open a fresh tunnel on a different port (`ssh -f -N -L 3101:...`) and run with `GRAPHQL_URL=http://localhost:3101`.
- **Schema drift tests fail on a fresh checkout**: your endpoint's schema version differs from the committed snapshot. Run `uv run pytest schema/ --update-schema` to rebaseline.

## More docs

- `README.md` — comprehensive framework reference.
- `performance/README.md` — deeper dive on load testing.
- `queries/regression/README.md` — how to add a new regression test.
- `.claude/skills/test-cardano-graphql.md` — Claude skill for invoking the framework from natural language.
