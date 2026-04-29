"""
Golden file load / save / compare plumbing.

Golden file schema:
{
  "test_name":            str,
  "description":          str,
  "network":              str,
  "query_file":           str,              # relative to tests/
  "variables":            {...},
  "anchor_block":         int | null,
  "ignore_paths":         [str, ...],       # dot.notation[*] — see utils/normalize
  "graphql_errors_allowed": bool,           # default false
  "expected":             {...} | null,     # populated by --update-golden
}
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from utils.diff import deep_diff, format_diff
from utils.normalize import apply_ignore_paths

TESTS_DIR = Path(__file__).parent.parent.resolve()


def load_golden(path: Path) -> dict[str, Any]:
    if not path.exists():
        raise FileNotFoundError(
            f"Golden not found: {path}\n"
            f"Run: uv run pytest --update-golden <test>  to capture it."
        )
    with open(path) as f:
        return json.load(f)


def save_golden(path: Path, golden: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w") as f:
        json.dump(golden, f, indent=2, sort_keys=True)
        f.write("\n")


def compare_or_update(
    golden_path: Path,
    golden: dict[str, Any],
    actual_response: dict[str, Any],
    update: bool,
) -> tuple[bool, str]:
    """
    Returns (passed, message). When passed is False, message contains the diff.

    When update=True, rewrites golden['expected'] to the normalized actual and
    returns (True, "updated").
    """
    ignore = list(golden.get("ignore_paths") or [])
    normalized_actual = apply_ignore_paths(actual_response, ignore)

    if update:
        golden["expected"] = normalized_actual
        save_golden(golden_path, golden)
        return True, f"updated: {golden_path.relative_to(TESTS_DIR)}"

    expected = golden.get("expected")
    if expected is None:
        return False, (
            f"Golden {golden_path.relative_to(TESTS_DIR)} has no 'expected' block. "
            "Run with --update-golden to capture."
        )

    normalized_expected = apply_ignore_paths(expected, ignore)
    diffs = deep_diff(normalized_expected, normalized_actual)
    if not diffs:
        return True, "match"
    return False, format_diff(diffs)
