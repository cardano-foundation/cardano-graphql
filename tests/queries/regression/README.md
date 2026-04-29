# Phase 4 — GitHub-issue regression tests

Each closed bug we want to guard against lives as:

1. `queries/regression/issue_<N>_<slug>.graphql` — the GraphQL that reproduces the original symptom.
2. `golden/functional/regression/issue_<N>_<slug>.json` — golden config with `regression_issue: <N>`.

These are picked up automatically by `functional/test_golden_examples.py` because they sit under `golden/functional/` — no separate runner needed.

## Seeded issues

| Issue | Title                                                                    | Fixed in  | What the test covers                                                                   |
|-------|--------------------------------------------------------------------------|-----------|----------------------------------------------------------------------------------------|
| #951  | allow nullable value in tx metadata                                       | #951 PR   | Tx metadata with nullable `value` must serialize; exercises a range of txs at anchor.  |
| #953  | nullable token                                                           | #953 PR   | Token quantities that are null must not break output serialization.                    |
| #978  | Background service failing with response ERR_STRING_TOO_LONG              | #980 PR   | Token-registry assets with large metadata must fetch cleanly; chunks work.             |
| #979  | backfill missing assets                                                  | #979 PR   | Backfilled asset rows must expose `tokenMints` and be queryable by `assetId`.          |

Capture and replay like any other golden:

```bash
uv run pytest -k regression --update-golden -v
uv run pytest -k regression -v
```

## Adding a new regression test

1. Find the issue's repro query. The commit/PR that closed the issue usually cites it. If not, construct a minimal query that would have hit the bug.
2. Save to `queries/regression/issue_<N>_<short-slug>.graphql` — use variables where anchor-block pinning is needed.
3. Add `golden/functional/regression/issue_<N>_<short-slug>.json`:
   ```json
   {
     "test_name": "issue_<N>_<slug>",
     "description": "Regression for cardano-foundation/cardano-graphql#<N> — <one-line what broke>.",
     "network": "mainnet",
     "query_file": "queries/regression/issue_<N>_<slug>.graphql",
     "variables": { ... },
     "anchor_block": 13228000,
     "ignore_paths": [],
     "graphql_errors_allowed": false,
     "regression_issue": <N>
   }
   ```
4. Capture: `uv run pytest 'functional/test_golden_examples.py::test_golden[regression__issue_<N>_<slug>]' --update-golden -v`.
5. Add a row to the table above.

## Bulk-seeding from the issue tracker

For initial seeding or periodic audits, the project has a lightweight helper:

```bash
uv run python performance/../queries/regression/fetch_issues.py --limit 20
```

(See `fetch_issues.py` for the implementation — it reads the unauthenticated
GitHub REST API with rate-limit awareness. It produces skeleton golden files;
a human still writes the repro query and captures the golden.)
