"""
Schema coverage computation.

Two granularities:

1. **Top-level coverage** — what fraction of `Query.*` fields are referenced by
   at least one query. This is the headline number.
2. **Field-level coverage** (`--full`) — uses graphql-core's TypeInfo visitor
   to walk every selection in every `.graphql` file. For each `OBJECT` /
   `INTERFACE` type in the SDL, reports how many of its fields are touched.
   Skips Hasura-generated `_aggregate` / `_aggregate_fields` / `*_bool_exp` /
   `*_order_by` / `aggregate` machinery — those types are infrastructure, not
   user-facing data.

CLI:
  uv run python -m utils.coverage              # top-level summary
  uv run python -m utils.coverage --full       # full field-level report
  uv run python -m utils.coverage --json       # machine-readable
  uv run python -m utils.coverage --allure DIR # write allure attachment

The full report is what `generate_report.py` injects into the HTML dashboard.
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any

from graphql import (
    DocumentNode,
    FieldNode,
    FragmentDefinitionNode,
    OperationDefinitionNode,
    SelectionSetNode,
    TypeInfo,
    Visitor,
    build_schema,
    parse,
    visit,
)
from graphql.utilities import TypeInfoVisitor

TESTS_DIR = Path(__file__).parent.parent.resolve()
QUERIES_DIR = TESTS_DIR / "queries"
SNAPSHOT_PATH = TESTS_DIR / "schema" / "snapshot.json"
REPO_ROOT = TESTS_DIR.parent
SCHEMA_SDL_PATH = REPO_ROOT / "packages" / "api-cardano-db-hasura" / "schema.graphql"


# --------------------------------------------------------------------------- #
# Top-level coverage (cheap, no schema build).                                #
# --------------------------------------------------------------------------- #

def _field_names(selection_set: SelectionSetNode | None) -> set[str]:
    if selection_set is None:
        return set()
    return {sel.name.value for sel in selection_set.selections if isinstance(sel, FieldNode)}


def top_level_fields(doc: DocumentNode) -> set[str]:
    out: set[str] = set()
    for defn in doc.definitions:
        if isinstance(defn, OperationDefinitionNode):
            out |= _field_names(defn.selection_set)
    return out


def walk_queries(root: Path = QUERIES_DIR) -> dict[str, set[str]]:
    result: dict[str, set[str]] = {}
    for path in sorted(root.rglob("*.graphql")):
        try:
            doc = parse(path.read_text())
            result[str(path.relative_to(TESTS_DIR))] = top_level_fields(doc)
        except Exception as e:  # noqa: BLE001
            result[str(path.relative_to(TESTS_DIR))] = set()
            print(f"[coverage] warn: failed to parse {path}: {e}", file=sys.stderr)
    return result


def load_query_root_fields(snapshot_path: Path = SNAPSHOT_PATH) -> dict[str, str]:
    if not snapshot_path.exists():
        return {}
    snap = json.loads(snapshot_path.read_text())
    query_type_name = snap.get("queryType") or "Query"
    query_type = (snap.get("types") or {}).get(query_type_name) or {}
    return {
        fname: fmeta.get("type") or "<unknown>"
        for fname, fmeta in (query_type.get("fields") or {}).items()
    }


def compute(queries_root: Path = QUERIES_DIR, snapshot_path: Path = SNAPSHOT_PATH) -> dict:
    per_file = walk_queries(queries_root)
    covered: set[str] = set()
    for fields in per_file.values():
        covered |= fields

    declared = set(load_query_root_fields(snapshot_path).keys())
    if not declared:
        return {
            "status": "no-snapshot",
            "covered_fields": sorted(covered),
            "note": "schema/snapshot.json missing — run `uv run pytest schema/ --update-schema` first.",
        }

    hit = covered & declared
    return {
        "status": "ok",
        "declared_query_fields": len(declared),
        "covered_query_fields": len(hit),
        "coverage_pct": round(100.0 * len(hit) / max(len(declared), 1), 1),
        "covered": sorted(hit),
        "missing": sorted(declared - covered),
        "outside_query_root": sorted(covered - declared),
        "per_file": {k: sorted(v) for k, v in per_file.items()},
    }


# --------------------------------------------------------------------------- #
# Full coverage — uses graphql-core TypeInfo to resolve field-on-type.        #
# --------------------------------------------------------------------------- #

# Hasura generates a lot of infra types; counting their fields against
# coverage produces a misleading denominator. Skip them.
def _is_infra_type(name: str) -> bool:
    return (
        name.startswith("__")
        or name.endswith("_aggregate")
        or name.endswith("_aggregate_fields")
        or name.endswith("_aggregate_order_by")
        or name.endswith("_bool_exp")
        or name.endswith("_comparison_exp")
        or name.endswith("_order_by")
        or name.endswith("_select_column")
        or name.endswith("_avg_fields")
        or name.endswith("_max_fields")
        or name.endswith("_min_fields")
        or name.endswith("_stddev_fields")
        or name.endswith("_stddev_pop_fields")
        or name.endswith("_stddev_samp_fields")
        or name.endswith("_sum_fields")
        or name.endswith("_var_pop_fields")
        or name.endswith("_var_samp_fields")
        or name.endswith("_variance_fields")
        or name in {"aggregate", "aggregate_orderBy", "Mutation", "mutation_root", "TransactionSubmitResponse"}
    )


class _Collector(Visitor):
    def __init__(self, type_info: TypeInfo) -> None:
        super().__init__()
        self.type_info = type_info
        self.refs: set[tuple[str, str]] = set()

    def enter_field(self, node: FieldNode, *_args: Any) -> None:
        parent_type = self.type_info.get_parent_type()
        field_def = self.type_info.get_field_def()
        if parent_type is not None and field_def is not None:
            self.refs.add((parent_type.name, node.name.value))


_SCHEMA_CACHE: dict[str, Any] = {}


def _get_schema(schema_sdl_path: Path = SCHEMA_SDL_PATH):
    """Load and cache the schema. Used by query_field_refs() for fast repeated calls."""
    key = str(schema_sdl_path)
    cached = _SCHEMA_CACHE.get(key)
    if cached is not None:
        return cached
    schema = build_schema(schema_sdl_path.read_text())
    _SCHEMA_CACHE[key] = schema
    return schema


def query_field_refs(query_text: str, schema_sdl_path: Path = SCHEMA_SDL_PATH) -> set[tuple[str, str]]:
    """Return the set of (parentTypeName, fieldName) pairs referenced in `query_text`.

    Used by conftest.py for per-root and CIP-26 / CIP-68 auto-markers.
    Returns an empty set on missing SDL or parse failure.
    """
    if not schema_sdl_path.exists():
        return set()
    try:
        schema = _get_schema(schema_sdl_path)
        type_info = TypeInfo(schema)
        doc = parse(query_text)
        v = _Collector(type_info)
        visit(doc, TypeInfoVisitor(type_info, v))
        return v.refs
    except Exception:
        return set()


def collect_field_refs(schema_sdl: str, queries_root: Path = QUERIES_DIR) -> dict:
    """Build the schema and visit every query, gathering (Type, field) tuples."""
    schema = build_schema(schema_sdl)
    type_info = TypeInfo(schema)

    refs: set[tuple[str, str]] = set()
    per_file_refs: dict[str, set[tuple[str, str]]] = {}
    parse_failures: list[str] = []

    for path in sorted(queries_root.rglob("*.graphql")):
        try:
            doc = parse(path.read_text())
            v = _Collector(type_info)
            visit(doc, TypeInfoVisitor(type_info, v))
            per_file_refs[str(path.relative_to(TESTS_DIR))] = v.refs
            refs |= v.refs
        except Exception as e:  # noqa: BLE001
            parse_failures.append(f"{path}: {e}")

    # Build per-type field maps from the schema (only object/interface).
    by_type: dict[str, dict[str, Any]] = {}
    for name, t in schema.type_map.items():
        if _is_infra_type(name):
            continue
        if not hasattr(t, "fields"):
            continue
        # Skip non-object types (input, enum, scalar, union)
        if not getattr(t, "fields", None):
            continue
        try:
            fields = list(t.fields.keys())
        except Exception:
            continue
        if not fields:
            continue
        by_type[name] = {
            "kind": t.__class__.__name__.replace("GraphQL", ""),
            "fields": fields,
        }

    # Compute per-type coverage.
    refs_by_type: dict[str, set[str]] = defaultdict(set)
    for typ, fld in refs:
        refs_by_type[typ].add(fld)

    rows: list[dict] = []
    total_fields = 0
    covered_fields = 0
    for type_name in sorted(by_type):
        decl = set(by_type[type_name]["fields"])
        cov = refs_by_type.get(type_name, set()) & decl
        rows.append({
            "type": type_name,
            "kind": by_type[type_name]["kind"],
            "declared": len(decl),
            "covered": len(cov),
            "pct": round(100.0 * len(cov) / max(len(decl), 1), 1),
            "missing": sorted(decl - cov),
        })
        total_fields += len(decl)
        covered_fields += len(cov)

    return {
        "status": "ok",
        "total_types_considered": len(by_type),
        "types_with_any_coverage": sum(1 for r in rows if r["covered"] > 0),
        "total_fields": total_fields,
        "covered_fields": covered_fields,
        "field_coverage_pct": round(100.0 * covered_fields / max(total_fields, 1), 1),
        "per_type": rows,
        "per_file_refs": {
            f: [{"type": t, "field": fld} for t, fld in sorted(rs)]
            for f, rs in per_file_refs.items()
        },
        "parse_failures": parse_failures,
    }


def compute_full(queries_root: Path = QUERIES_DIR,
                 schema_sdl_path: Path = SCHEMA_SDL_PATH,
                 snapshot_path: Path = SNAPSHOT_PATH) -> dict:
    """Combine top-level + field-level coverage into one report."""
    top = compute(queries_root, snapshot_path)
    if not schema_sdl_path.exists():
        return {**top, "field_level": {"status": "no-sdl",
                                        "note": f"missing {schema_sdl_path}"}}
    sdl = schema_sdl_path.read_text()
    full = collect_field_refs(sdl, queries_root)
    return {**top, "field_level": full}


# --------------------------------------------------------------------------- #
# Pretty printing                                                              #
# --------------------------------------------------------------------------- #

def _format_top(report: dict) -> str:
    if report.get("status") == "no-snapshot":
        return f"Schema coverage: snapshot missing — {report.get('note')}"
    pct = report["coverage_pct"]
    out = [
        f"Top-level Query coverage: {report['covered_query_fields']}/"
        f"{report['declared_query_fields']} ({pct}%)",
    ]
    if report["missing"]:
        out.append(f"  Missing roots: {', '.join(report['missing'])}")
    return "\n".join(out)


def _format_field(field: dict) -> str:
    if field.get("status") == "no-sdl":
        return f"Field-level coverage: {field.get('note')}"
    out = [
        "",
        f"Field-level coverage: {field['covered_fields']}/{field['total_fields']} fields "
        f"({field['field_coverage_pct']}%) across {field['types_with_any_coverage']}/"
        f"{field['total_types_considered']} types touched",
        "",
        "Top types by coverage:",
    ]
    rows_sorted = sorted(field["per_type"], key=lambda r: (-r["pct"], -r["declared"]))
    out.append(f"  {'TYPE':<28} {'KIND':<10} {'COV':>6} {'TOTAL':>6} {'PCT':>6}")
    for r in rows_sorted[:25]:
        out.append(
            f"  {r['type']:<28} {r['kind']:<10} {r['covered']:>6} {r['declared']:>6} {r['pct']:>5}%"
        )
    out.append("")
    out.append("Untouched types (no field referenced):")
    untouched = [r for r in field["per_type"] if r["covered"] == 0]
    untouched.sort(key=lambda r: -r["declared"])
    for r in untouched[:25]:
        out.append(f"  - {r['type']} ({r['kind']}, {r['declared']} fields)")
    if len(untouched) > 25:
        out.append(f"  ... ({len(untouched) - 25} more)")
    return "\n".join(out)


def _format(report: dict, full: bool) -> str:
    parts = [_format_top(report)]
    if full and "field_level" in report:
        parts.append(_format_field(report["field_level"]))
    return "\n".join(parts)


# --------------------------------------------------------------------------- #
# CLI                                                                          #
# --------------------------------------------------------------------------- #

def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--full", action="store_true", help="Include field-level report")
    ap.add_argument("--json", action="store_true", help="Emit JSON")
    ap.add_argument("--allure", default=None, help="Write text attachment to allure dir")
    ap.add_argument("--snapshot", default=None)
    ap.add_argument("--queries", default=None)
    ap.add_argument("--schema-sdl", default=None)
    args = ap.parse_args(argv)

    snap = Path(args.snapshot).resolve() if args.snapshot else SNAPSHOT_PATH
    qroot = Path(args.queries).resolve() if args.queries else QUERIES_DIR
    sdl = Path(args.schema_sdl).resolve() if args.schema_sdl else SCHEMA_SDL_PATH

    report = compute_full(qroot, sdl, snap) if args.full else compute(qroot, snap)

    if args.json:
        print(json.dumps(report, indent=2, sort_keys=True, default=list))
        return 0

    text = _format(report, full=args.full)
    print(text)

    if args.allure:
        out = Path(args.allure) / "schema_coverage.txt"
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(text)
        print(f"\n[coverage] wrote {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
