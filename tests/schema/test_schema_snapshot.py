"""
Phase 5 — schema introspection snapshot diff.

Fetches the running endpoint's introspection, canonicalizes it, and diffs
against schema/snapshot.json. Any divergence fails the test.

To (re-)bootstrap the snapshot after an intentional schema change:

    uv run pytest schema/ --update-schema
"""

from __future__ import annotations

from pathlib import Path

import pytest

from utils.schema_snapshot import (
    INTROSPECTION_QUERY,
    canonicalize,
    diff_snapshots,
    load_snapshot,
    save_snapshot,
)

TESTS_DIR = Path(__file__).parent.parent.resolve()
SNAPSHOT_PATH = TESTS_DIR / "schema" / "snapshot.json"


pytestmark = [pytest.mark.schema]


@pytest.fixture(scope="session")
def update_schema(request):
    return bool(request.config.getoption("--update-schema"))


def test_schema_matches_snapshot(client, update_schema):
    resp = client.execute(INTROSPECTION_QUERY)
    if resp.errors:
        pytest.fail(f"introspection query failed: {resp.errors}")

    actual = canonicalize(resp.json_body)

    if update_schema:
        save_snapshot(SNAPSHOT_PATH, actual)
        return

    if not SNAPSHOT_PATH.exists():
        pytest.fail(
            f"Missing {SNAPSHOT_PATH.relative_to(TESTS_DIR)}. "
            "Run: uv run pytest schema/ --update-schema  to capture it."
        )

    expected = load_snapshot(SNAPSHOT_PATH)
    diffs = diff_snapshots(expected, actual)
    if diffs:
        body = "\n".join(f"  - {d}" for d in diffs[:80])
        more = "" if len(diffs) <= 80 else f"\n  ... ({len(diffs) - 80} more)"
        pytest.fail(
            "Schema drift detected. If intentional, refresh via:\n"
            "  uv run pytest schema/ --update-schema\n"
            "Differences:\n" + body + more
        )
