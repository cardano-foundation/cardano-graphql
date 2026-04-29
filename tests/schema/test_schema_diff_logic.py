"""
Offline unit tests for utils/schema_snapshot.py — no endpoint needed.

These prove diff_snapshots() catches the four kinds of schema regression that
matter for clients: added field, removed field, nullability flip, removed enum
value. If any of these asserts fail, the Phase 5 gate is not doing its job.
"""

from __future__ import annotations

import pytest

from utils.schema_snapshot import canonicalize, diff_snapshots


pytestmark = [pytest.mark.schema]


def _type_obj(name: str, fields: list[dict]) -> dict:
    return {
        "kind": "OBJECT",
        "name": name,
        "fields": fields,
        "inputFields": None,
        "enumValues": None,
        "possibleTypes": None,
        "interfaces": None,
    }


def _enum(name: str, values: list[str]) -> dict:
    return {
        "kind": "ENUM",
        "name": name,
        "fields": None,
        "inputFields": None,
        "enumValues": [{"name": v, "isDeprecated": False} for v in values],
        "possibleTypes": None,
        "interfaces": None,
    }


def _f(name: str, type_name: str, non_null: bool = False) -> dict:
    t = {"kind": "SCALAR", "name": type_name, "ofType": None}
    if non_null:
        t = {"kind": "NON_NULL", "name": None, "ofType": t}
    return {"name": name, "type": t, "args": []}


def _schema(types: list[dict]) -> dict:
    return {
        "data": {
            "__schema": {
                "queryType": {"name": "Query"},
                "mutationType": None,
                "subscriptionType": None,
                "types": types,
            }
        }
    }


def test_detects_removed_field():
    v1 = _schema([_type_obj("Block", [_f("hash", "String"), _f("forgedAt", "DateTime")])])
    v2 = _schema([_type_obj("Block", [_f("hash", "String")])])
    diffs = diff_snapshots(canonicalize(v1), canonicalize(v2))
    assert any("Block.forgedAt removed" in d for d in diffs), diffs


def test_detects_added_field():
    v1 = _schema([_type_obj("Block", [_f("hash", "String")])])
    v2 = _schema([_type_obj("Block", [_f("hash", "String"), _f("size", "Int")])])
    diffs = diff_snapshots(canonicalize(v1), canonicalize(v2))
    assert any("Block.size added" in d for d in diffs), diffs


def test_detects_nullability_flip():
    v1 = _schema([_type_obj("Block", [_f("number", "Int", non_null=False)])])
    v2 = _schema([_type_obj("Block", [_f("number", "Int", non_null=True)])])
    diffs = diff_snapshots(canonicalize(v1), canonicalize(v2))
    assert any("Int -> Int!" in d for d in diffs), diffs


def test_detects_removed_enum_value():
    v1 = _schema([_enum("OrderDir", ["ASC", "DESC"])])
    v2 = _schema([_enum("OrderDir", ["ASC"])])
    diffs = diff_snapshots(canonicalize(v1), canonicalize(v2))
    assert any("DESC (enum) removed" in d for d in diffs), diffs


def test_identical_snapshots_return_no_diffs():
    s = _schema([_type_obj("Block", [_f("hash", "String", non_null=True)])])
    diffs = diff_snapshots(canonicalize(s), canonicalize(s))
    assert diffs == [], diffs


def test_ignores_introspection_meta_types():
    # Double-underscore types are GraphQL introspection meta-types; they churn
    # independently of the user-facing schema.
    meta = {
        "kind": "OBJECT", "name": "__Schema", "fields": None,
        "inputFields": None, "enumValues": None, "possibleTypes": None, "interfaces": None,
    }
    canonical = canonicalize(_schema([meta]))
    assert "__Schema" not in canonical["types"]
