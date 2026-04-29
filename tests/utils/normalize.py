"""
Response normalization for golden-file comparison.

Removes fields from a response JSON at dot-notation paths with [*] for every list
element. Used to blank out volatile fields (timestamps, aggregate counts that
change with chain progression, token-registry metadata when registry state drifts)
before diffing.

Path DSL:
  "data.blocks[*].forgedAt"          -> every block's forgedAt
  "data.cardano.tip.hash"            -> exact scalar
  "data.transactions[*].inputs[*].txId"  -> nested wildcard
"""

from __future__ import annotations

import copy
import re
from typing import Any

_PATH_TOKEN = re.compile(r"([^.\[]+)(\[\*\])?")


def _tokenize(path: str) -> list[tuple[str, bool]]:
    """Turn 'a.b[*].c' into [('a', False), ('b', True), ('c', False)]."""
    tokens: list[tuple[str, bool]] = []
    for match in _PATH_TOKEN.finditer(path):
        name, wildcard = match.group(1), bool(match.group(2))
        tokens.append((name, wildcard))
    return tokens


def _remove(node: Any, tokens: list[tuple[str, bool]]) -> None:
    if not tokens or node is None:
        return
    name, wildcard = tokens[0]
    rest = tokens[1:]

    if not isinstance(node, dict):
        return
    if name not in node:
        return

    if not rest:
        # Leaf: drop the key entirely
        del node[name]
        return

    child = node[name]
    if wildcard:
        if isinstance(child, list):
            for item in child:
                _remove(item, rest)
        return
    _remove(child, rest)


def apply_ignore_paths(payload: dict[str, Any], paths: list[str]) -> dict[str, Any]:
    """
    Return a deep copy of payload with every path removed.

    We remove (rather than mask) so missing-vs-masked mismatches can't sneak in.
    """
    out = copy.deepcopy(payload)
    for p in paths or []:
        _remove(out, _tokenize(p))
    return out
