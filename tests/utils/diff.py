"""
Deep diff for golden-file comparison.

Adapted from cardano-rosetta-java/tests/integration/test_golden_examples.py —
returns a list of human-readable diff strings, empty if equal.

Diff semantics:
- Dicts: missing keys, unexpected keys, recursive value mismatch.
- Lists: length mismatch + elementwise recurse.
- Scalars: equality.
- Type mismatches are reported.
"""

from __future__ import annotations

from typing import Any


def deep_diff(expected: Any, actual: Any, path: str = "") -> list[str]:
    if type(expected) is not type(actual):
        # Allow int/float coexistence (GraphQL Int can round-trip as either)
        if not (isinstance(expected, (int, float)) and isinstance(actual, (int, float))):
            return [
                f"{path or '<root>'}: type mismatch — expected "
                f"{type(expected).__name__}, got {type(actual).__name__}"
            ]

    if isinstance(expected, dict):
        diffs: list[str] = []
        for k in expected.keys() - actual.keys():
            diffs.append(f"{_p(path, k)}: missing in actual")
        for k in actual.keys() - expected.keys():
            diffs.append(f"{_p(path, k)}: unexpected in actual")
        for k in expected.keys() & actual.keys():
            diffs.extend(deep_diff(expected[k], actual[k], _p(path, k)))
        return diffs

    if isinstance(expected, list):
        if len(expected) != len(actual):
            return [
                f"{path or '<root>'}: list length mismatch — "
                f"expected {len(expected)}, got {len(actual)}"
            ]
        diffs = []
        for i, (e, a) in enumerate(zip(expected, actual)):
            diffs.extend(deep_diff(e, a, f"{path}[{i}]"))
        return diffs

    if expected != actual:
        return [f"{path or '<root>'}: expected {expected!r}, got {actual!r}"]
    return []


def _p(parent: str, key: str) -> str:
    return f"{parent}.{key}" if parent else key


def format_diff(diffs: list[str], limit: int = 40) -> str:
    """Short human-readable multi-line diff block. Truncates beyond `limit`."""
    if not diffs:
        return "<no diff>"
    head = diffs[:limit]
    suffix = "" if len(diffs) <= limit else f"\n  ... ({len(diffs) - limit} more)"
    return "\n".join(f"  - {d}" for d in head) + suffix
