"""
Phase 3 — multi-instance comparison.

Runs every Phase 1 golden's query against two GraphQL endpoints and asserts the
normalized responses are equivalent. This is the tool for proving a second
implementation of the cardano-graphql schema returns identical data for the same
pinned inputs.

Run:

    uv run pytest multi_instance/ \
        --compare-url=http://other-instance:3100 \
        --tb=short -v

If --compare-url is not provided, the whole module is skipped so default runs
(sanity + golden + negative) remain unaffected.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from client.graphql_client import GraphQLClient
from utils.diff import deep_diff, format_diff
from utils.golden_io import load_golden
from utils.normalize import apply_ignore_paths
from utils.query_loader import load_query_text

TESTS_DIR = Path(__file__).parent.parent.resolve()
GOLDEN_DIR = TESTS_DIR / "golden" / "functional"


GOLDEN_PATHS = sorted(
    p for p in GOLDEN_DIR.rglob("*.json") if p.is_file()
) if GOLDEN_DIR.exists() else []


def _case_id(path: Path) -> str:
    return str(path.relative_to(GOLDEN_DIR)).removesuffix(".json").replace("/", "__")


pytestmark = [pytest.mark.multi_instance]


@pytest.fixture(scope="session")
def compare_url(request):
    url = request.config.getoption("--compare-url")
    if not url:
        pytest.skip("--compare-url not provided; skipping multi-instance compare tests")
    return url


@pytest.fixture
def compare_client(compare_url, network_config, request):
    """Recording client against the second instance, distinct from the primary."""
    timeout = network_config.get("timeout_s", 180)
    with GraphQLClient(compare_url, timeout_s=timeout) as c:
        original = c.execute

        def recording_execute(query, variables=None, operation_name=None):
            resp = original(query, variables=variables, operation_name=operation_name)
            request.node._recorded_responses.append((
                {"method": "POST", "url": f"{c.base_url}/graphql",
                 "body": {"query": query, "variables": variables or {}}},
                {"status_code": resp.status_code, "body": resp.json_body},
            ))
            return resp

        c.execute = recording_execute
        yield c


@pytest.mark.parametrize(
    "golden_path",
    GOLDEN_PATHS,
    ids=[_case_id(p) for p in GOLDEN_PATHS] or ["<no-goldens>"],
)
def test_primary_matches_secondary(
    client,              # primary — from root conftest.py
    compare_client,      # secondary — defined above
    golden_path,
    network_name,
):
    """Same query, two instances, normalized diff must be empty."""
    if not GOLDEN_PATHS:
        pytest.skip("No golden files discovered under tests/golden/functional/")

    golden = load_golden(golden_path)
    cfg_network = golden.get("network")
    if cfg_network and cfg_network != network_name:
        pytest.skip(f"golden network={cfg_network}, running network={network_name}")

    query = load_query_text(golden["query_file"])
    variables = golden.get("variables") or {}

    primary = client.execute(query, variables=variables)
    secondary = compare_client.execute(query, variables=variables)

    if primary.errors:
        pytest.fail(f"primary instance returned errors: {primary.errors}")
    if secondary.errors:
        pytest.fail(f"secondary instance returned errors: {secondary.errors}")

    ignore = list(golden.get("ignore_paths") or [])
    a = apply_ignore_paths({"data": primary.data}, ignore)
    b = apply_ignore_paths({"data": secondary.data}, ignore)

    diffs = deep_diff(a, b)
    if diffs:
        pytest.fail(
            f"instances diverged for {golden_path.name} "
            f"(primary vs {compare_client.base_url}):\n{format_diff(diffs)}"
        )
