"""
Parametrized golden-file runner.

Discovers every JSON under tests/golden/functional/**.json and turns each
into a pytest case. The config file names the query file (relative to tests/),
the variables to pass, and any ignore_paths for volatile fields.

Run the captured set:
  uv run pytest functional/test_golden_examples.py -v

Bootstrap or refresh goldens (reviewer must diff before committing):
  uv run pytest functional/test_golden_examples.py --update-golden
"""

from __future__ import annotations

from pathlib import Path

import pytest

from utils.golden_io import compare_or_update, load_golden
from utils.query_loader import load_query_text

TESTS_DIR = Path(__file__).parent.parent.resolve()
GOLDEN_DIR = TESTS_DIR / "golden" / "functional"


def _discover_goldens() -> list[Path]:
    if not GOLDEN_DIR.exists():
        return []
    return sorted(p for p in GOLDEN_DIR.rglob("*.json") if p.is_file())


def _case_id(path: Path) -> str:
    return str(path.relative_to(GOLDEN_DIR)).removesuffix(".json").replace("/", "__")


GOLDEN_PATHS = _discover_goldens()


pytestmark = [pytest.mark.golden]


@pytest.mark.parametrize(
    "golden_path",
    GOLDEN_PATHS,
    ids=[_case_id(p) for p in GOLDEN_PATHS] or ["<no-goldens>"],
)
def test_golden(client, golden_path, update_golden, network_name):
    if not GOLDEN_PATHS:
        pytest.skip("No golden files discovered under tests/golden/functional/")

    golden = load_golden(golden_path)

    cfg_network = golden.get("network")
    if cfg_network and cfg_network != network_name:
        pytest.skip(f"golden network={cfg_network}, running network={network_name}")

    query_text = load_query_text(golden["query_file"])
    variables = golden.get("variables") or {}

    resp = client.execute(query_text, variables=variables)

    if resp.errors and not golden.get("graphql_errors_allowed", False):
        pytest.fail(
            f"Unexpected GraphQL errors:\n" +
            "\n".join(f"  - {e.get('message')}" for e in resp.errors)
        )

    assert resp.status_code == 200

    actual = {"data": resp.data, "errors": resp.errors or None}
    # Strip errors key when absent to keep goldens clean for the common path.
    if actual["errors"] is None:
        del actual["errors"]

    passed, message = compare_or_update(
        golden_path, golden, actual, update=update_golden
    )
    if not passed:
        pytest.fail(f"Golden diff for {golden_path.name}:\n{message}")
