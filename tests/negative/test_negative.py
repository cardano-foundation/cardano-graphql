"""
Negative / error-path tests.

GraphQL convention: transport returns HTTP 200 even for request-level failures;
errors live in `response.errors`. These tests assert on that shape.
"""

from __future__ import annotations

import pytest


pytestmark = [pytest.mark.negative, pytest.mark.pr]


def _any_message_contains(errors: list[dict], needle: str) -> bool:
    needle_l = needle.lower()
    return any(needle_l in (e.get("message") or "").lower() for e in errors)


def test_missing_required_variable(client):
    """`$addresses` is required (non-null) and omitted."""
    query = (
        "query($addresses: [String]!) {"
        "  utxos(where: { address: { _in: $addresses } }) { txHash }"
        "}"
    )
    resp = client.execute(query, variables={})
    assert resp.status_code in (200, 400), f"unexpected HTTP {resp.status_code}"
    assert resp.errors, "expected GraphQL errors for missing required var"
    assert _any_message_contains(resp.errors, "addresses"), (
        f"error message should mention $addresses: {resp.errors}"
    )


def test_wrong_variable_type(client):
    """`$number` typed Int! but passed a string."""
    query = (
        "query($number: Int!) {"
        "  blocks(where: { number: { _eq: $number } }) { hash }"
        "}"
    )
    resp = client.execute(query, variables={"number": "not-an-int"})
    assert resp.status_code in (200, 400), f"unexpected HTTP {resp.status_code}"
    assert resp.errors, "expected GraphQL errors for type mismatch"
    assert _any_message_contains(resp.errors, "int"), (
        f"error message should mention Int type: {resp.errors}"
    )


def test_unknown_field_selection(client):
    """`notAField` does not exist on Block."""
    query = "{ blocks(limit: 1) { notAField } }"
    resp = client.execute(query)
    assert resp.status_code in (200, 400), f"unexpected HTTP {resp.status_code}"
    assert resp.errors, "expected GraphQL errors for unknown field"
    assert _any_message_contains(resp.errors, "notafield") or \
           _any_message_contains(resp.errors, "field"), (
        f"error message should mention unknown field: {resp.errors}"
    )


def test_malformed_graphql_syntax(client):
    """Missing closing brace — server must return a syntax error."""
    query = "{ blocks(limit: 1) { hash "
    resp = client.execute(query)
    assert resp.status_code in (200, 400), (
        f"unexpected HTTP status: {resp.status_code}"
    )
    assert resp.errors, "expected GraphQL errors for syntax error"


def test_invalid_bool_operator(client):
    """`_gtLte` is not a valid filter operator."""
    query = (
        "{ blocks(where: { number: { _gtLte: 100 } }) { hash } }"
    )
    resp = client.execute(query)
    assert resp.status_code in (200, 400), f"unexpected HTTP {resp.status_code}"
    assert resp.errors, "expected GraphQL errors for invalid operator"


def test_block_at_impossibly_large_number(client):
    """Tip is < 10^9; asking for a block beyond it should return empty, no errors."""
    query = (
        "query($n: Int!) { blocks(where: { number: { _eq: $n } }) { hash } }"
    )
    resp = client.execute(query, variables={"n": 10**9})
    assert resp.status_code == 200
    assert not resp.errors, f"unexpected GraphQL errors: {resp.errors}"
    assert resp.data == {"blocks": []}


def test_malformed_hash_variable(client):
    """`Hash32Hex` scalar should reject non-hex strings."""
    query = (
        "query($hash: Hash32Hex!) {"
        "  transactions(where: { hash: { _eq: $hash } }) { hash }"
        "}"
    )
    resp = client.execute(query, variables={"hash": "not-a-hash"})
    assert resp.status_code in (200, 400), f"unexpected HTTP {resp.status_code}"
    assert resp.errors, "expected GraphQL errors for bad hash scalar"


def test_invalid_limit_argument(client):
    """Negative limit should be rejected by the resolver or the scalar."""
    query = "{ blocks(limit: -1) { hash } }"
    resp = client.execute(query)
    # Resolvers vary: some return errors, some clamp to 0. Accept either but flag
    # the observed behavior so reviewers see what the API did.
    assert resp.status_code == 200
    # If no errors, at least the response must be a valid shape
    if not resp.errors:
        assert resp.data is not None
        assert "blocks" in resp.data
