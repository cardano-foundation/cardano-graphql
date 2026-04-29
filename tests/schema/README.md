# Phase 5 — schema introspection diff

Detects breaking schema changes by canonicalizing the running endpoint's introspection result and diffing against a committed snapshot.

```
schema/
├── snapshot.json                 # canonical, sorted snapshot — version-controlled
├── test_schema_snapshot.py       # pytest test that hits the live endpoint and diffs
└── test_schema_diff_logic.py     # offline unit tests for the diff function (no endpoint)
```

The diff helper lives in `utils/schema_snapshot.py`.

---

## Run it

```bash
# Compare live introspection against snapshot.json (no endpoint changes ⇒ pass)
uv run pytest schema/ -v

# Capture / refresh the snapshot when an intentional schema change ships
uv run pytest schema/test_schema_snapshot.py --update-schema -v
git diff tests/schema/snapshot.json
git commit -am "refresh schema snapshot for <feature>"
```

**Forbidden in CI:** `--update-schema` is blocked when `CI=true`. Snapshots are reviewed in PRs, never auto-rewritten.

## What's detected

Every kind of breaking change a client would notice:

| Change | Caught? |
|---|---|
| Field removed (`Block.forgedAt`) | ✓ |
| Field added (`Block.size`) | ✓ |
| Nullability flip (`Int` → `Int!`, or the other direction) | ✓ |
| Argument added / removed / type-changed | ✓ |
| Enum value removed (`OrderDir.DESC`) | ✓ |
| Enum value added | ✓ |
| Input-field added / removed / type-changed | ✓ |
| Type added / removed / kind-changed (object → interface, etc.) | ✓ |

What's **not** detected by design:

- Description / docstring changes (noisy, not breaking).
- Deprecation reason changes for fields not previously deprecated.
- Reordering of fields within a type (sorted before diff).
- Addition of new types unless something references them.

## Output on a real drift

```
Schema drift detected. If intentional, refresh via:
  uv run pytest schema/ --update-schema
Differences:
  - Block.forgedAt removed
  - Block.size added
  - Block.number type: Int -> Int!
  - OrderDir.DESC (enum) removed
```

Each line points at a specific `Type.field` or `Type` change. Failures are loud and actionable.

## Two test files, two purposes

- **`test_schema_snapshot.py`** — *integration* test. Hits the live endpoint, runs the canonical introspection query, diffs against `snapshot.json`. Slow (~500ms with one network round-trip). Marker: `schema`.
- **`test_schema_diff_logic.py`** — *unit* tests for the diff function itself. No endpoint, no I/O, runs in 10ms. Verifies `diff_snapshots()` correctly catches add/remove/nullability/enum changes. Marker: `schema`.

The unit tests guard against the diff logic itself regressing — for example, if someone refactors `_is_infra_type()` and accidentally hides a real change.

## Snapshot format

Compact, sorted, comparable. Excludes Hasura infrastructure (`*_aggregate`, `*_bool_exp`, `*_comparison_exp`, `*_order_by`, etc. — see `_is_infra_type()` in `utils/schema_snapshot.py`) so the diff focuses on the user-facing schema.

```json
{
  "queryType": "Query",
  "mutationType": "Mutation",
  "subscriptionType": null,
  "types": {
    "Block": {
      "kind": "OBJECT",
      "fields": {
        "epoch":  { "type": "Epoch",     "args": {} },
        "fees":   { "type": "Lovelace!", "args": {} },
        "hash":   { "type": "Hash32Hex!","args": {} }
      }
    }
  }
}
```

Fields and argument names within each type are alphabetically sorted at canonicalization time, so reordering in the source schema doesn't produce noise.

## Scaling note

Snapshot is ~4,700 lines / 110 KB on mainnet's schema. Diff is whole-file string-comparable in git, so PRs surface schema changes inline.
