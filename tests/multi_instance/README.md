# Phase 3 — multi-instance comparison

Drives every golden's query against **two** GraphQL endpoints and asserts the responses are equivalent (after `ignore_paths` masking). This is the tool for proving a second implementation of the cardano-graphql schema returns identical data for the same pinned inputs.

Why it matters: cardano-graphql exists in multiple variants (the Cardano Foundation server, downstream forks, alternate Hasura configurations). Schema-level conformance isn't enough — two services can return different *data* for the same query against the same chain state. This phase catches that.

---

## Run it

```bash
# 1. The primary endpoint comes from GRAPHQL_URL (or networks.yaml)
# 2. Open a tunnel / point the secondary at a different instance
ssh -f -N -L 3101:localhost:3100 my-other-instance

# 3. Run only the multi-instance tests
uv run pytest multi_instance/ --compare-url=http://localhost:3101 -v
```

Without `--compare-url`, every test in this directory is **automatically skipped** with the message `--compare-url not provided; skipping multi-instance compare tests`. So default `pytest` runs aren't affected.

## What it does per test

For every golden under `tests/golden/functional/**/*.json`:

1. Send the golden's query to the **primary** endpoint (`GRAPHQL_URL` / `networks.yaml`).
2. Send the same query and variables to the **secondary** endpoint (`--compare-url`).
3. Apply the golden's `ignore_paths` mask to **both** responses.
4. Deep-diff the two normalized responses with `utils/diff.py`.
5. Fail if any non-ignored path differs, with a path-by-path diff showing primary vs secondary.

This deliberately reuses Phase 1 goldens — every golden is a parametrized case in this module too, so the test count automatically grows as the golden corpus grows.

## What it doesn't do

- It does **not** compare against the golden's `expected` block — that's the Phase 1 job. Here we only compare two live instances against each other.
- It does **not** check schema introspection equality — that's `tests/schema/`.

## When the test fails

The diff output points at exact JSON paths that differ:

```
instances diverged for first20Blocks.json (primary vs http://localhost:3101):
  - data.blocks[3].hash: expected 'e39d988dd…', got 'aa11bb22cc…'
  - data.blocks[5].number: type mismatch — expected int, got str
```

Investigate from the server side: usually it's an index missing on the secondary, a different db-sync version, or stale data on one node.

## Pairing with goldens

Auto-markers (`-m assets`, `-m cip26`, etc.) work here too — every multi_instance test inherits the markers of its underlying golden's query. So you can do:

```bash
uv run pytest multi_instance/ --compare-url=... -m "assets or cip26" -v
```

to compare just the asset-related queries.

## Limits / known issues

- 8 preprod-tagged goldens auto-skip when running mainnet primary (network-filter logic in `test_compare.py`). Same skip rule as `functional/test_golden_examples.py`.
- Compare currently exits on the first diff per test; it does not enumerate every divergent test in one go (that's pytest's default — use `--maxfail=...` to control).
- No latency comparison yet — both instances are queried sequentially. If you want to measure relative latency, use the Phase 2 Locust harness against each URL separately and diff the CSVs.
