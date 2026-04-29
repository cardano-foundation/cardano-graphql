"""
Tiny shared loader for performance/ — reads tests/config/networks.yaml and
picks the network via CARDANO_NETWORK (default mainnet). Reuses Phase 1's
query_loader so the .graphql files stay the single source of truth.

Kept here (instead of imported from tests/utils) because locust workers run
outside pytest's sys.path rigging in conftest.py.
"""

from __future__ import annotations

import os
from functools import lru_cache
from pathlib import Path

import yaml

TESTS_DIR = Path(__file__).parent.parent.resolve()


@lru_cache(maxsize=1)
def load_network() -> dict:
    path = TESTS_DIR / "config" / "networks.yaml"
    with open(path) as f:
        all_nets = yaml.safe_load(f)
    name = os.environ.get("CARDANO_NETWORK", "mainnet")
    if name not in all_nets:
        raise RuntimeError(
            f"network '{name}' not in {path} (have: {list(all_nets)})"
        )
    cfg = all_nets[name]
    cfg["_name"] = name
    return cfg


def load_query_text(rel_path: str) -> str:
    full = (TESTS_DIR / rel_path).resolve()
    if not str(full).startswith(str(TESTS_DIR)):
        raise ValueError(f"query path {rel_path!r} escapes tests/")
    return full.read_text()
