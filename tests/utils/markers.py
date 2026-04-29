#!/usr/bin/env python3
"""
List the auto-applied markers, grouped by query root and CIP, so you know what
`-m <name>` filters are available.

Usage:
  uv run python -m utils.markers              # one-line summary per marker
  uv run python -m utils.markers --tests      # also list tests under each marker
  uv run python -m utils.markers --json       # machine-readable
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.resolve()))

from utils.coverage import query_field_refs
from utils.golden_io import load_golden
from utils.query_loader import load_query_text

TESTS_DIR = Path(__file__).parent.parent.resolve()
GOLDEN_DIR = TESTS_DIR / "golden" / "functional"

CIP26_ASSET_FIELDS = {
    "name", "ticker", "description", "logo", "url", "decimals", "metadataHash",
}
CIP68_DATUM_FIELDS = {"value", "bytes"}


def compute() -> dict[str, list[str]]:
    """Return marker -> sorted list of golden test ids that carry the marker."""
    out: dict[str, set[str]] = defaultdict(set)
    for golden_path in sorted(GOLDEN_DIR.rglob("*.json")):
        rel = str(golden_path.relative_to(GOLDEN_DIR)).removesuffix(".json").replace("/", "__")
        try:
            golden = load_golden(golden_path)
            query_text = load_query_text(golden["query_file"])
            refs = query_field_refs(query_text)
        except Exception:
            continue

        for type_name, field in refs:
            if type_name == "Query":
                out[field].add(rel)

        if any(t == "Asset" and f in CIP26_ASSET_FIELDS for t, f in refs):
            out["cip26"].add(rel)
        if any(t == "Datum" and f in CIP68_DATUM_FIELDS for t, f in refs):
            out["cip68"].add(rel)

    return {k: sorted(v) for k, v in sorted(out.items())}


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--tests", action="store_true", help="List tests under each marker")
    ap.add_argument("--json", action="store_true", help="Emit JSON")
    args = ap.parse_args(argv)

    by_marker = compute()

    if args.json:
        print(json.dumps(by_marker, indent=2, sort_keys=True))
        return 0

    print("Auto-applied markers (use with `pytest -m <name>`):\n")
    print(f"  {'MARKER':<30} TESTS")
    print(f"  {'-' * 30} -----")
    for name, tests in sorted(by_marker.items(), key=lambda kv: (-len(kv[1]), kv[0])):
        kind = "[CIP]" if name.startswith("cip") else "[root]"
        print(f"  {kind} {name:<24} {len(tests)} test{'s' if len(tests) != 1 else ''}")
        if args.tests:
            for t in tests:
                print(f"        - {t}")
            print()
    print()
    print("Also available (manually applied):")
    print("  pr / nightly / smoke / slow / golden / negative / sanity")
    print("  multi_instance / regression / schema / requires_token_registry")
    print()
    print("To run everything (no marker filter):  uv run pytest")
    return 0


if __name__ == "__main__":
    sys.exit(main())
