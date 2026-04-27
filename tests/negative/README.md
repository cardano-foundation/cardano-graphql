# Negative tests

Eight error-path tests that prove the GraphQL layer rejects bad input correctly. Run in <3s — these are PR-gate fast.

```bash
uv run pytest negative/ -v
uv run pytest -m negative
```

## What each test asserts

| Test | What it sends | What it checks |
|---|---|---|
| `test_missing_required_variable` | `$addresses: [String]!` query but no `addresses` variable | `errors[]` mentions the missing var |
| `test_wrong_variable_type` | String passed where `Int!` expected | `errors[]` mentions Int |
| `test_unknown_field_selection` | `{ blocks(limit:1) { notAField } }` | `errors[]` (validation rejects unknown field) |
| `test_malformed_graphql_syntax` | Unclosed brace | HTTP 200 or 400; non-empty `errors[]` |
| `test_invalid_bool_operator` | `where: { number: { _gtLte: 100 } }` | `errors[]` mentions invalid operator |
| `test_block_at_impossibly_large_number` | `_eq: 10^9` (past tip) | HTTP 200, `errors == []`, `data.blocks == []` (graceful empty) |
| `test_malformed_hash_variable` | `Hash32Hex!` scalar with non-hex value | `errors[]` for scalar coercion failure |
| `test_invalid_limit_argument` | `limit: -1` | accepts either a clean error or empty result, but the response shape must remain valid |

## Why it matters

GraphQL convention says servers can return errors via either:

- **HTTP 4xx** (Apollo Server returns 400 for parse/validation errors), or
- **HTTP 200 with non-empty `errors[]`** (for execution-phase errors).

These tests accept **both** for the failure cases (`assert resp.status_code in (200, 400)`) but always require `resp.errors` to be non-empty when an error is expected. This was an actual regression early in the framework — a test asserting only `status == 200` missed all of Apollo's validation errors.

The single "graceful empty" case (`test_block_at_impossibly_large_number`) inverts the assertion: an out-of-range filter should return `data.blocks == []` with no errors, not crash.

## How to add a new negative test

1. Construct a query that exercises a specific failure mode (e.g., null on non-null arg, fragment cycle, deep nesting).
2. Decide whether errors are expected or not.
3. Assert on `resp.status_code` (allow 200 or 400) and `resp.errors`.

Keep new tests under 20 lines; the test file is meant to be scannable.

## Running a single test

```bash
uv run pytest negative/test_negative.py::test_unknown_field_selection -v
```
