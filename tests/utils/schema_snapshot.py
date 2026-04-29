"""
Phase 5 — canonical schema snapshot + diff.

We persist a simplified, sorted snapshot of the GraphQL schema (from an
introspection query) so PRs touching the schema are forced to also update the
snapshot — making breaking changes visible in code review instead of hiding in
server code.

Simplified ≠ lossless: we reduce introspection's full __schema to a compact
representation of types/fields/args/enum values. It keeps the signal that
matters for client compatibility (name, kind, required-ness, arg shape) and
drops what doesn't (descriptions, deprecation reasons for undeprecated items).
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

INTROSPECTION_QUERY = """
query IntrospectionQuery {
  __schema {
    queryType { name }
    mutationType { name }
    subscriptionType { name }
    types {
      kind
      name
      fields(includeDeprecated: true) {
        name
        args { name type { ...TypeRef } }
        type { ...TypeRef }
        isDeprecated
      }
      inputFields {
        name
        type { ...TypeRef }
      }
      interfaces { name }
      enumValues(includeDeprecated: true) {
        name
        isDeprecated
      }
      possibleTypes { name }
    }
  }
}
fragment TypeRef on __Type {
  kind
  name
  ofType {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType { kind name }
      }
    }
  }
}
""".strip()


def _type_ref_str(t: dict | None) -> str:
    """Flatten a nested TypeRef into its GraphQL-notation string (e.g. [Int!]!)."""
    if t is None:
        return "<null>"
    kind = t.get("kind")
    if kind == "NON_NULL":
        return _type_ref_str(t.get("ofType")) + "!"
    if kind == "LIST":
        return "[" + _type_ref_str(t.get("ofType")) + "]"
    return t.get("name") or f"<{kind}>"


def canonicalize(introspection: dict) -> dict:
    """Turn raw introspection into a stable, sorted, compact dict."""
    schema = introspection["data"]["__schema"]
    out: dict[str, Any] = {
        "queryType": (schema.get("queryType") or {}).get("name"),
        "mutationType": (schema.get("mutationType") or {}).get("name"),
        "subscriptionType": (schema.get("subscriptionType") or {}).get("name"),
        "types": {},
    }

    for t in schema.get("types") or []:
        name = t.get("name")
        if not name or name.startswith("__"):
            continue  # skip introspection meta-types
        kind = t.get("kind")
        entry: dict[str, Any] = {"kind": kind}

        if kind in ("OBJECT", "INTERFACE") and t.get("fields"):
            entry["fields"] = {}
            for f in sorted(t["fields"], key=lambda x: x["name"]):
                entry["fields"][f["name"]] = {
                    "type": _type_ref_str(f.get("type")),
                    "args": {
                        a["name"]: _type_ref_str(a.get("type"))
                        for a in sorted(f.get("args") or [], key=lambda x: x["name"])
                    },
                    **({"deprecated": True} if f.get("isDeprecated") else {}),
                }

        if kind == "INPUT_OBJECT" and t.get("inputFields"):
            entry["inputFields"] = {
                i["name"]: _type_ref_str(i.get("type"))
                for i in sorted(t["inputFields"], key=lambda x: x["name"])
            }

        if kind == "ENUM" and t.get("enumValues"):
            entry["enumValues"] = sorted(
                v["name"] for v in t["enumValues"] if not v.get("isDeprecated")
            )

        if kind == "UNION" and t.get("possibleTypes"):
            entry["possibleTypes"] = sorted(
                p["name"] for p in t["possibleTypes"] if p.get("name")
            )

        if kind == "INTERFACE" and t.get("possibleTypes"):
            entry["implementedBy"] = sorted(
                p["name"] for p in t["possibleTypes"] if p.get("name")
            )

        out["types"][name] = entry

    return out


def diff_snapshots(expected: dict, actual: dict) -> list[str]:
    """Return human-readable lines describing every meaningful schema change."""
    diffs: list[str] = []

    for k in ("queryType", "mutationType", "subscriptionType"):
        if expected.get(k) != actual.get(k):
            diffs.append(
                f"{k}: expected {expected.get(k)!r}, got {actual.get(k)!r}"
            )

    exp_types = expected.get("types") or {}
    act_types = actual.get("types") or {}

    for name in sorted(set(exp_types) - set(act_types)):
        diffs.append(f"type '{name}' removed")
    for name in sorted(set(act_types) - set(exp_types)):
        diffs.append(f"type '{name}' added")

    for name in sorted(set(exp_types) & set(act_types)):
        diffs.extend(_diff_type(name, exp_types[name], act_types[name]))

    return diffs


def _diff_type(name: str, exp: dict, act: dict) -> list[str]:
    out: list[str] = []
    if exp.get("kind") != act.get("kind"):
        out.append(f"type '{name}' kind: {exp.get('kind')} -> {act.get('kind')}")

    # Fields
    exp_f = exp.get("fields") or {}
    act_f = act.get("fields") or {}
    for f in sorted(set(exp_f) - set(act_f)):
        out.append(f"{name}.{f} removed")
    for f in sorted(set(act_f) - set(exp_f)):
        out.append(f"{name}.{f} added")
    for f in sorted(set(exp_f) & set(act_f)):
        ef, af = exp_f[f], act_f[f]
        if ef.get("type") != af.get("type"):
            out.append(f"{name}.{f} type: {ef.get('type')} -> {af.get('type')}")
        if ef.get("args") != af.get("args"):
            out.append(f"{name}.{f} args: {ef.get('args')} -> {af.get('args')}")

    # Input fields
    exp_i = exp.get("inputFields") or {}
    act_i = act.get("inputFields") or {}
    for f in sorted(set(exp_i) - set(act_i)):
        out.append(f"{name}.{f} (input) removed")
    for f in sorted(set(act_i) - set(exp_i)):
        out.append(f"{name}.{f} (input) added")
    for f in sorted(set(exp_i) & set(act_i)):
        if exp_i[f] != act_i[f]:
            out.append(f"{name}.{f} (input) type: {exp_i[f]} -> {act_i[f]}")

    # Enum values
    exp_e = set(exp.get("enumValues") or [])
    act_e = set(act.get("enumValues") or [])
    for v in sorted(exp_e - act_e):
        out.append(f"{name}.{v} (enum) removed")
    for v in sorted(act_e - exp_e):
        out.append(f"{name}.{v} (enum) added")

    return out


def save_snapshot(path: Path, snapshot: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w") as f:
        json.dump(snapshot, f, indent=2, sort_keys=True)
        f.write("\n")


def load_snapshot(path: Path) -> dict:
    with open(path) as f:
        return json.load(f)
