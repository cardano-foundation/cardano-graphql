"""
Session-wide fixtures and Allure hooks for the cardano-graphql test framework.

Adapted from cardano-rosetta-java/tests/data-endpoints/conftest.py — stripped of
pruning/Rosetta specifics, with GraphQL client + readiness fixture + --update-golden
plumbing added.
"""

import ast
import inspect
import json
import os
import subprocess
import sys
import textwrap
import time
from pathlib import Path

import allure
import pytest
import yaml
from dotenv import find_dotenv, load_dotenv

# Allow tests to import client/... and utils/... as top-level modules.
TESTS_DIR = Path(__file__).parent.resolve()
for p in (str(TESTS_DIR), str(TESTS_DIR / "client"), str(TESTS_DIR / "utils")):
    if p not in sys.path:
        sys.path.insert(0, p)

from client.graphql_client import GraphQLClient  # noqa: E402

load_dotenv(find_dotenv(), override=True)


# ---------------------------------------------------------------------------
# CLI options
# ---------------------------------------------------------------------------

def pytest_addoption(parser):
    parser.addoption(
        "--update-golden",
        action="store_true",
        default=False,
        help=(
            "Capture live GraphQL responses into golden/**/*.json and pass the test. "
            "Forbidden when CI=true to prevent accidental CI overwrites."
        ),
    )
    parser.addoption(
        "--compare-url",
        action="store",
        default=None,
        help=(
            "Secondary GraphQL URL for Phase 3 multi-instance comparison. "
            "When unset, multi_instance tests are skipped."
        ),
    )
    parser.addoption(
        "--update-schema",
        action="store_true",
        default=False,
        help=(
            "Phase 5: refresh schema/snapshot.json from the primary endpoint's introspection."
        ),
    )


def pytest_configure(config):
    if config.getoption("--update-golden") and os.environ.get("CI", "").lower() == "true":
        raise pytest.UsageError(
            "--update-golden is forbidden when CI=true. "
            "Capture goldens locally and commit them via PR review."
        )

    # Dynamically register one marker per top-level Query field, plus cip26 / cip68.
    # This lets users do:
    #   uv run pytest -m assets         # all golden tests touching the `assets` root
    #   uv run pytest -m cip26          # token-registry metadata coverage
    #   uv run pytest -m cip68          # datum-embedded metadata coverage
    snap_path = TESTS_DIR / "schema" / "snapshot.json"
    if snap_path.exists():
        try:
            snap = json.loads(snap_path.read_text())
            roots = (snap.get("types", {}).get("Query", {}).get("fields") or {})
            for f in roots:
                config.addinivalue_line(
                    "markers", f"{f}: auto — golden tests whose query touches Query.{f}"
                )
        except Exception:
            pass
    config.addinivalue_line(
        "markers", "cip26: golden tests exercising CIP-26 token-registry metadata "
                   "(Asset.{name,ticker,description,logo,url,decimals,metadataHash})"
    )
    config.addinivalue_line(
        "markers", "cip68: golden tests exercising CIP-68 datum-embedded metadata "
                   "(Datum.{value,bytes})"
    )


# ---------------------------------------------------------------------------
# Auto-markers — derived from each golden's query AST at collection time
# ---------------------------------------------------------------------------

CIP26_ASSET_FIELDS = {
    "name", "ticker", "description", "logo", "url", "decimals", "metadataHash",
}
CIP68_DATUM_FIELDS = {"value", "bytes"}


def pytest_collection_modifyitems(config, items):
    """Tag each parametrized golden test with markers derived from its query.

    For each test where the parametrize id is a golden file path:
      * Add `pytest.mark.<rootField>` for every top-level `Query.<rootField>` referenced.
      * Add `pytest.mark.cip26` if any `Asset.<token-registry-field>` is selected.
      * Add `pytest.mark.cip68` if any `Datum.value` / `Datum.bytes` is selected.
    Silently skips items that don't carry a `golden_path` parameter — no-op for sanity / negative.
    """
    try:
        from utils.coverage import query_field_refs
        from utils.golden_io import load_golden
        from utils.query_loader import load_query_text
    except Exception:
        return  # graphql-core or utils not available — skip auto-marking

    for item in items:
        callspec = getattr(item, "callspec", None)
        if not callspec:
            continue
        golden_path = callspec.params.get("golden_path")
        if golden_path is None:
            continue
        try:
            golden = load_golden(Path(golden_path))
            query_text = load_query_text(golden["query_file"])
            refs = query_field_refs(query_text)
        except Exception:
            continue

        for type_name, field in refs:
            if type_name == "Query":
                # Add the per-root marker. pytest is happy as long as the marker
                # was registered in pytest_configure above.
                item.add_marker(getattr(pytest.mark, field))

        if any(t == "Asset" and f in CIP26_ASSET_FIELDS for t, f in refs):
            item.add_marker(pytest.mark.cip26)
        if any(t == "Datum" and f in CIP68_DATUM_FIELDS for t, f in refs):
            item.add_marker(pytest.mark.cip68)


# ---------------------------------------------------------------------------
# Allure session metadata
# ---------------------------------------------------------------------------

def pytest_sessionstart(session):
    allure_dir = session.config.option.allure_report_dir
    if not allure_dir:
        return

    repo_root = Path(__file__).parent.parent

    def _git(cmd):
        try:
            return subprocess.check_output(
                ["git", "-C", str(repo_root)] + cmd,
                text=True, stderr=subprocess.DEVNULL,
            ).strip()
        except Exception:
            return "unknown"

    env = {
        "Release.Version": _git(["describe", "--tags", "--always"]),
        "Git.Branch": _git(["rev-parse", "--abbrev-ref", "HEAD"]),
        "Git.Commit": _git(["rev-parse", "--short", "HEAD"]),
        "CARDANO_NETWORK": os.environ.get("CARDANO_NETWORK", ""),
        "GRAPHQL_URL": os.environ.get("GRAPHQL_URL", ""),
        "Update.Golden": str(session.config.getoption("--update-golden")),
    }
    props_path = Path(allure_dir) / "environment.properties"
    props_path.parent.mkdir(parents=True, exist_ok=True)
    with open(props_path, "w") as f:
        for k, v in env.items():
            f.write(f"{k}={v}\n")


# ---------------------------------------------------------------------------
# Per-test request/response/assertion attachments
# ---------------------------------------------------------------------------

@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    outcome = yield
    report = outcome.get_result()
    if report.when != "call":
        return

    for i, (req, resp) in enumerate(getattr(item, "_recorded_responses", [])):
        allure.attach(
            body=json.dumps(req, indent=2, default=str),
            name=f"Request #{i + 1} - {req.get('url', '')}",
            attachment_type=allure.attachment_type.JSON,
        )
        allure.attach(
            body=json.dumps(resp, indent=2, default=str),
            name=f"Response #{i + 1} - HTTP {resp.get('status_code', '?')}",
            attachment_type=allure.attachment_type.JSON,
        )

    for name, body in getattr(item, "_recorded_attachments", []):
        allure.attach(body=body, name=name, attachment_type=allure.attachment_type.TEXT)

    try:
        source = textwrap.dedent(inspect.getsource(item.obj))
        lines = source.splitlines()
        tree = ast.parse(source)
        assert_lines = sorted({
            node.lineno for node in ast.walk(tree) if isinstance(node, ast.Assert)
        })
        if assert_lines:
            allure.attach(
                body="\n".join(lines[n - 1] for n in assert_lines),
                name="Assertions",
                attachment_type=allure.attachment_type.TEXT,
            )
    except Exception:
        pass


# ---------------------------------------------------------------------------
# Core fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(scope="session")
def network_name():
    name = os.environ.get("CARDANO_NETWORK")
    if not name:
        raise RuntimeError(
            "CARDANO_NETWORK environment variable is required. "
            "Default is set in pytest.ini (mainnet); override via env to switch."
        )
    return name


@pytest.fixture(scope="session")
def networks_config():
    path = TESTS_DIR / "config" / "networks.yaml"
    if not path.exists():
        raise FileNotFoundError(f"Missing {path}")
    with open(path) as f:
        return yaml.safe_load(f)


@pytest.fixture(scope="session")
def network_config(networks_config, network_name):
    if network_name not in networks_config:
        available = ", ".join(networks_config.keys())
        raise RuntimeError(
            f"Network '{network_name}' not in networks.yaml. Available: {available}"
        )
    return networks_config[network_name]


@pytest.fixture(scope="session")
def graphql_url(network_config):
    return os.environ.get("GRAPHQL_URL") or network_config["url"]


@pytest.fixture(scope="session")
def anchor_block(network_config):
    return network_config["anchor_block"]


@pytest.fixture(scope="session")
def update_golden(request):
    return bool(request.config.getoption("--update-golden"))


@pytest.fixture(scope="session")
def sync_readiness(graphql_url, network_config):
    """
    Poll { cardanoDbMeta { initialized syncPercentage } } with exponential backoff.
    Fail the session if the instance is not initialized and >= 99% synced.

    The anchor_block must also be <= current tip.
    """
    timeout_s = network_config.get("timeout_s", 30)
    attempts = 9
    backoff = 2.0
    delay = 1.0

    last_err = None
    with GraphQLClient(graphql_url, timeout_s=timeout_s) as c:
        for i in range(attempts):
            try:
                data = c.raw_query(
                    "{ cardanoDbMeta { initialized syncPercentage } "
                    "cardano { tip { number } } }"
                )
                meta = data["data"]["cardanoDbMeta"]
                tip = data["data"]["cardano"]["tip"]["number"]
                if meta["initialized"] and float(meta["syncPercentage"]) >= 99.0:
                    if tip < network_config["anchor_block"]:
                        raise RuntimeError(
                            f"Chain tip {tip} < anchor_block "
                            f"{network_config['anchor_block']}. "
                            "Wait for sync or lower anchor_block in networks.yaml."
                        )
                    return {"tip": tip, "sync_percentage": meta["syncPercentage"]}
                last_err = f"not ready: initialized={meta['initialized']} " \
                           f"sync={meta['syncPercentage']}"
            except Exception as e:  # noqa: BLE001
                last_err = str(e)
            time.sleep(delay)
            delay *= backoff

    raise RuntimeError(
        f"GraphQL endpoint {graphql_url} failed readiness after {attempts} attempts: "
        f"{last_err}"
    )


@pytest.fixture
def client(graphql_url, network_config, sync_readiness, request):
    """
    Per-test recording GraphQL client. Every call is captured and attached to Allure.
    """
    with GraphQLClient(graphql_url, timeout_s=network_config.get("timeout_s", 30)) as c:
        request.node._recorded_responses = []
        request.node._recorded_attachments = []
        original = c.execute

        def recording_execute(query, variables=None, operation_name=None):
            resp = original(query, variables=variables, operation_name=operation_name)
            body = {"query": query, "variables": variables or {}}
            if operation_name:
                body["operationName"] = operation_name
            request.node._recorded_responses.append((
                {"method": "POST", "url": f"{c.base_url}/graphql", "body": body},
                {"status_code": resp.status_code, "body": resp.json_body},
            ))
            return resp

        c.execute = recording_execute
        yield c


def attach_text(item, name: str, body: str) -> None:
    """Helper for tests/utils to attach extra TEXT blobs (e.g. diff) to Allure."""
    getattr(item, "_recorded_attachments", []).append((name, body))
