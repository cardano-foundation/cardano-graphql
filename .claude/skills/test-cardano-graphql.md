---
name: test-cardano-graphql
description: |
  Run the cardano-graphql Python test framework at tests/.
  Use when the user asks to "run the graphql tests", "test cardano-graphql", "capture goldens", "run load test", "check schema drift", "compare two graphql instances", or similar.

  Arguments are free-form natural language; parse them into the matching pytest
  / uv invocation. If the user names a specific phase, scope the run:
    * sanity / smoke           → `uv run pytest -m sanity -v`
    * goldens / functional     → `uv run pytest functional/ -v`
    * negative / errors        → `uv run pytest negative/ -v`
    * load / perf / benchmark  → `uv run python performance/run_load.py --profile=<smoke|soak|stability>`
    * schema drift / introspection → `uv run pytest schema/test_schema_snapshot.py -v`
    * coverage                 → `uv run python -m utils.coverage`
    * compare instances        → `uv run pytest multi_instance/ --compare-url=<url> -v`
    * update goldens (never in CI) → `uv run pytest --update-golden -v`
    * everything               → `uv run pytest -v`
  Extract target network (mainnet/preprod) into CARDANO_NETWORK and custom URL into GRAPHQL_URL.
---

# cardano-graphql test framework — invocation skill

This skill helps run the Python test framework at `tests/` (repo-root relative). The framework pins queries to a fixed anchor block, runs against any GraphQL endpoint matching the cardano-graphql schema, and produces Allure reports.

## Quick decision tree

1. **Parse the user's request** for intent keywords:
   - `run`, `test`, `check` → execute tests
   - `capture`, `update goldens`, `refresh` → `--update-golden` flow (forbidden when `CI=true`)
   - `compare`, `diff two instances` → multi-instance mode (needs `--compare-url=<url>`)
   - `load`, `perf`, `benchmark`, `stress` → Locust harness
   - `coverage`, `what's tested` → coverage report
   - `baseline`, `p95`, `latency gate` → `check_baselines.py`
2. **Parse target network**:
   - If the user says "preprod", set `CARDANO_NETWORK=preprod`.
   - If they say "mainnet" or omit it, default to `CARDANO_NETWORK=mainnet`.
3. **Parse endpoint**:
   - `localhost:3100` or unspecified → `GRAPHQL_URL=http://localhost:3100` (default).
   - User gives a URL → `GRAPHQL_URL=<their url>`.
   - User mentions `ssh mainnet` and the tunnel is slow → suggest opening a fresh tunnel first: `ssh -f -N -L 3101:localhost:3100 mainnet`, then `GRAPHQL_URL=http://localhost:3101`.
4. **Run** from `tests/`:
   ```bash
   cd tests/
   uv sync                      # first time only
   <invocation>                 # see table below
   ```
5. **For a persistent HTML report**, append `--alluredir=reports/allure-raw` and surface: `allure serve reports/allure-raw` for an interactive dashboard, or `allure generate reports/allure-raw -o reports/allure-html --clean` for a static site.

## Canonical invocations

| User says… | Run this |
|---|---|
| "run sanity" / "is the endpoint up" | `uv run pytest -m sanity -v` |
| "run goldens" / "run functional tests" | `uv run pytest functional/ -v` |
| "run negative" / "test error paths" | `uv run pytest negative/ -v` |
| "run everything" / "full suite" / "run all tests" | `uv run pytest -v --alluredir=reports/allure-raw` (bare pytest is "all") |
| "run all asset tests" / "test the assets root" | `uv run pytest -m assets -v` |
| "run all <root> tests" (any of the 52 Query roots) | `uv run pytest -m <root> -v` |
| "run CIP-26 tests" / "token registry coverage" | `uv run pytest -m cip26 -v` |
| "run CIP-68 tests" / "datum metadata" | `uv run pytest -m cip68 -v` |
| "what markers / what filters are available" | `uv run python -m utils.markers` |
| "capture goldens" / "refresh goldens" | `uv run pytest --update-golden -v` (never with `CI=true`) |
| "load test smoke" / "perf smoke" | `uv run python performance/run_load.py --profile=smoke` |
| "soak test" | `uv run python performance/run_load.py --profile=soak` |
| "stability / capacity test" | `uv run python performance/run_load.py --profile=stability` |
| "check perf baselines" | `uv run python performance/check_baselines.py reports/perf-<prefix>_stats.csv` |
| "compare two instances X and Y" | `GRAPHQL_URL=X uv run pytest multi_instance/ --compare-url=Y -v` |
| "check schema drift" / "has schema changed?" | `uv run pytest schema/ -v` |
| "update schema snapshot" | `uv run pytest schema/test_schema_snapshot.py --update-schema -v` |
| "coverage" / "what fraction is tested" | `uv run python -m utils.coverage` |
| "run PR gate" | `uv run pytest -m pr -v` |

## Markers (for -m filters)

**Manually applied (in test files):**
- `sanity` — cardanoDbMeta / tip / genesis / anchor block checks.
- `golden` — golden-file comparison tests.
- `negative` — error-path tests.
- `pr` — fast must-pass on PR.
- `nightly` — comprehensive (scheduled).
- `smoke` — non-blocking data availability.
- `slow` — >5s tests.
- `multi_instance` — cross-URL compare (auto-skipped without `--compare-url`).
- `schema` — schema introspection diff.
- `regression` — goldens seeded from GitHub issues.
- `requires_token_registry` — needs `TOKEN_REGISTRY_ENABLED` on the instance.

**Auto-applied (from each query's AST at collection time):**
- One marker per top-level `Query.<root>` field — every one of the 52 roots
  (`assets`, `blocks`, `transactions`, `paymentAddresses`, `stakePools`,
  `epochs`, `delegationVotes`, `committeeMembers`, `votingAnchor`,
  `*_aggregate` variants, etc.).
  Use `-m <root>` to target only goldens whose query touches that root.
- `cip26` — golden tests selecting CIP-26 token-registry metadata
  (`Asset.{name,ticker,description,logo,url,decimals,metadataHash}`).
- `cip68` — golden tests selecting CIP-68 datum-embedded metadata
  (`Datum.{value,bytes}`).

To enumerate every available marker and which tests it matches, run:
```bash
uv run python -m utils.markers           # summary
uv run python -m utils.markers --tests   # with per-marker test list
```

**"Run all tests"**: simply `uv run pytest` (no `-m`). There is no `-m all`
marker — pytest's default with no filter is "everything collected".

**Combine markers**: pytest supports boolean expressions:
```bash
uv run pytest -m "assets or transactions"      # union
uv run pytest -m "cip26 and not slow"          # intersection
uv run pytest -m "assets and not regression"   # difference
```

## Address-bucket flags (performance only)

`uv run python performance/run_load.py --load-profile=<light|full>`:
- `light` — drops medium and heavy bucket tasks; good against slow endpoints.
- `full` — includes all buckets; default.

## Safety rails

- **Never pass `--update-golden` when `CI=true`** — the framework refuses it on purpose. Captures happen locally, are reviewed in git diff, and committed in a PR.
- **Use a fresh SSH tunnel** if `localhost:3100` stalls. The VS Code Remote-SSH helper has been known to leave hung connections; a manual `ssh -f -N -L <local>:localhost:3100 mainnet` is faster to debug.
- **Heavy queries** (`paymentAddressSummary` on the `heavy` address, full-UTxO-set queries on active addresses) can take 60-180s on deployments without optimal indexes. Respect `timeout_s` in `config/networks.yaml` and bump it per network as needed.
- **Allure serve** opens a local web server; `allure generate` produces a self-contained static HTML directory — prefer generate for CI artefacts.

## Example sessions

### "test this against preprod"
```bash
cd tests/
CARDANO_NETWORK=preprod GRAPHQL_URL=http://localhost:3102 uv run pytest -v
```

### "compare local vs ssh mainnet"
```bash
cd tests/
ssh -f -N -L 3101:localhost:3100 mainnet
uv run pytest multi_instance/ --compare-url=http://localhost:3101 -v
```

### "I changed schema.graphql, check what broke"
```bash
cd tests/
uv run pytest schema/ -v
# if intentional: uv run pytest schema/ --update-schema -v  then commit
```

### "I added a query, check coverage went up"
```bash
cd tests/
uv run python -m utils.coverage
```

### "show me a browser report"
```bash
cd tests/
uv run pytest --alluredir=reports/allure-raw
allure serve reports/allure-raw
```
