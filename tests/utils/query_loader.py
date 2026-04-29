"""
Load .graphql query files from tests/queries/ by name.
"""

from __future__ import annotations

from functools import lru_cache
from pathlib import Path

TESTS_DIR = Path(__file__).parent.parent.resolve()
QUERIES_DIR = TESTS_DIR / "queries"


class QueryNotFound(FileNotFoundError):
    pass


@lru_cache(maxsize=None)
def load_query_text(rel_path: str) -> str:
    """
    Load a GraphQL query by path relative to tests/.

    Example: load_query_text("queries/example/first20Blocks.graphql")
    """
    path = (TESTS_DIR / rel_path).resolve()
    if not str(path).startswith(str(TESTS_DIR)):
        raise ValueError(f"Query path {rel_path!r} escapes tests/")
    if not path.exists():
        raise QueryNotFound(f"Query not found: {path}")
    return path.read_text()


def discover_queries(subdir: str = "") -> list[Path]:
    """
    Return all .graphql files under tests/queries/<subdir>/, recursively.
    """
    root = QUERIES_DIR / subdir if subdir else QUERIES_DIR
    if not root.exists():
        return []
    return sorted(p for p in root.rglob("*.graphql") if p.is_file())
