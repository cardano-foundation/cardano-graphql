#!/usr/bin/env python3
"""
Generate a single, self-contained HTML test report.

Runs pytest with pytest-html (one HTML file with per-test status + duration),
then injects:
  - A "scenario summary" banner at the top (counts, pass/fail, run duration)
  - A "schema coverage" section below the test table (from utils/coverage.py)
  - A "query corpus" appendix (one row per .graphql file in queries/)

Open reports/report.html in a browser.

Usage:
  uv run python generate_report.py              # run everything
  uv run python generate_report.py -- -m pr     # forward args after `--` to pytest
  uv run python generate_report.py --skip-tests # only regenerate coverage/appendix panels
"""

from __future__ import annotations

import argparse
import html
import json
import os
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.resolve()))
from utils import coverage as cov  # noqa: E402

TESTS_DIR = Path(__file__).parent.resolve()
REPORTS_DIR = TESTS_DIR / "reports"
REPORT_PATH = REPORTS_DIR / "report.html"


def run_pytest(pytest_args: list[str]) -> int:
    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    cmd = [
        "uv", "run", "pytest",
        "--html", str(REPORT_PATH),
        "--self-contained-html",
        "-v",
        "--tb=short",
        *pytest_args,
    ]
    print("[report] " + " ".join(cmd))
    return subprocess.call(cmd, cwd=str(TESTS_DIR))


def _query_corpus() -> list[dict]:
    """List every query file with top-level fields it touches."""
    qroot = TESTS_DIR / "queries"
    per_file = cov.walk_queries(qroot)
    rows: list[dict] = []
    for path_str, fields in sorted(per_file.items()):
        rows.append({
            "path": path_str,
            "fields": sorted(fields),
        })
    return rows


def _build_coverage_panel() -> str:
    report = cov.compute_full()
    if report.get("status") == "no-snapshot":
        return (
            "<section><h2>Schema coverage</h2>"
            "<p><em>Snapshot missing. Run "
            "<code>uv run pytest schema/ --update-schema</code>.</em></p></section>"
        )

    top_pct = report["coverage_pct"]
    top_bar_color = "#4caf50" if top_pct >= 80 else "#ffc107" if top_pct >= 50 else "#f44336"

    fl = report.get("field_level", {})
    has_fl = isinstance(fl, dict) and fl.get("status") == "ok"
    fl_pct = fl.get("field_coverage_pct", 0) if has_fl else 0
    fl_bar_color = "#4caf50" if fl_pct >= 80 else "#ffc107" if fl_pct >= 50 else "#f44336"

    covered_top = "".join(f"<li>{html.escape(f)}</li>" for f in report["covered"])
    missing_top = "".join(f"<li>{html.escape(f)}</li>" for f in report["missing"])

    field_section = ""
    if has_fl:
        rows_sorted = sorted(fl["per_type"], key=lambda r: (-r["pct"], -r["declared"]))
        rows_html = ""
        for r in rows_sorted:
            color = "#4caf50" if r["pct"] >= 80 else "#ffc107" if r["pct"] >= 50 else "#f44336"
            miss_attr = ""
            if r["missing"]:
                miss_attr = f' title="missing: {html.escape(", ".join(r["missing"][:10]))}"'
            rows_html += (
                f'<tr{miss_attr}>'
                f'<td><code>{html.escape(r["type"])}</code></td>'
                f'<td>{html.escape(r["kind"])}</td>'
                f'<td style="text-align:right">{r["covered"]}/{r["declared"]}</td>'
                f'<td style="width:120px"><div style="background:#eee;border-radius:3px;height:10px;width:100%"><div style="background:{color};height:100%;border-radius:3px;width:{r["pct"]}%"></div></div></td>'
                f'<td style="text-align:right">{r["pct"]}%</td>'
                f'</tr>'
            )
        field_section = f"""
<section style="margin:2em 0; font-family:-apple-system,Helvetica,Arial,sans-serif">
  <h2>Field-level coverage</h2>
  <div style="margin:0.5em 0">
    <strong>{fl['covered_fields']}</strong> of <strong>{fl['total_fields']}</strong> data-type fields covered
    across <strong>{fl['types_with_any_coverage']}/{fl['total_types_considered']}</strong> types —
    <strong>{fl_pct}%</strong>
  </div>
  <div style="background:#eee; border-radius:4px; height:18px; width:100%; margin:0.5em 0">
    <div style="background:{fl_bar_color}; height:100%; border-radius:4px; width:{fl_pct}%"></div>
  </div>
  <p style="font-size:0.85em; color:#555">
    Hasura infrastructure types (<code>*_aggregate</code>, <code>*_bool_exp</code>, <code>*_comparison_exp</code>, <code>*_order_by</code>, etc.) are excluded from the denominator. Hover any row for the missing field list.
  </p>
  <details>
    <summary><strong>Per-type breakdown ({len(rows_sorted)} types)</strong></summary>
    <table style="border-collapse:collapse; width:100%; font-size:0.85em; margin-top:0.5em">
      <thead>
        <tr style="background:#f5f5f5">
          <th style="text-align:left; padding:4px; border-bottom:2px solid #ccc">Type</th>
          <th style="text-align:left; padding:4px; border-bottom:2px solid #ccc">Kind</th>
          <th style="text-align:right; padding:4px; border-bottom:2px solid #ccc">Covered</th>
          <th style="padding:4px; border-bottom:2px solid #ccc">&nbsp;</th>
          <th style="text-align:right; padding:4px; border-bottom:2px solid #ccc">%</th>
        </tr>
      </thead>
      <tbody>{rows_html}</tbody>
    </table>
  </details>
</section>
"""

    return f"""
<section style="margin:2em 0; font-family:-apple-system,Helvetica,Arial,sans-serif">
  <h2>Schema coverage — top-level Query roots</h2>
  <div style="margin:0.5em 0">
    <strong>{report['covered_query_fields']}</strong> of
    <strong>{report['declared_query_fields']}</strong>
    top-level <code>Query</code> fields covered —
    <strong>{top_pct}%</strong>
  </div>
  <div style="background:#eee; border-radius:4px; height:18px; width:100%; margin:0.5em 0">
    <div style="background:{top_bar_color}; height:100%; border-radius:4px; width:{top_pct}%"></div>
  </div>
  <details open>
    <summary><strong>Covered ({len(report['covered'])})</strong></summary>
    <ul style="columns:3; font-family:monospace; font-size:0.85em">{covered_top}</ul>
  </details>
  <details>
    <summary><strong>Missing ({len(report['missing'])})</strong></summary>
    <ul style="columns:3; font-family:monospace; font-size:0.85em">{missing_top or "<li><em>none</em></li>"}</ul>
  </details>
</section>
{field_section}
"""


def _build_corpus_panel() -> str:
    rows_html = ""
    for row in _query_corpus():
        field_list = ", ".join(f"<code>{html.escape(f)}</code>" for f in row["fields"]) or "—"
        rows_html += (
            f"<tr><td><code>{html.escape(row['path'])}</code></td>"
            f"<td>{field_list}</td></tr>"
        )
    return f"""
<section style="margin:2em 0; font-family:-apple-system,Helvetica,Arial,sans-serif">
  <h2>Query corpus</h2>
  <p>One row per <code>.graphql</code> file under <code>tests/queries/</code>,
  listing top-level <code>Query</code> fields it references.</p>
  <table style="border-collapse:collapse; width:100%; font-size:0.9em">
    <thead>
      <tr style="background:#f5f5f5">
        <th style="text-align:left; padding:6px; border-bottom:2px solid #ccc">Query file</th>
        <th style="text-align:left; padding:6px; border-bottom:2px solid #ccc">Top-level fields</th>
      </tr>
    </thead>
    <tbody>{rows_html}</tbody>
  </table>
</section>
"""


def _build_header_panel(ts_start: float, ts_end: float) -> str:
    dur = ts_end - ts_start
    return f"""
<section style="margin:1em 0; font-family:-apple-system,Helvetica,Arial,sans-serif">
  <div style="background:#f0f7ff; border-left:4px solid #2196f3; padding:12px 16px; border-radius:2px">
    <h1 style="margin:0 0 0.3em 0">cardano-graphql — test report</h1>
    <div style="font-size:0.9em; color:#555">
      Generated at {time.strftime('%Y-%m-%d %H:%M:%S %Z', time.localtime(ts_end))}
      &middot; run time {dur:.1f}s
      &middot; network=<code>{html.escape(os.environ.get('CARDANO_NETWORK', 'mainnet'))}</code>
      &middot; endpoint=<code>{html.escape(os.environ.get('GRAPHQL_URL', 'http://localhost:3100'))}</code>
    </div>
  </div>
</section>
"""


def inject_panels(report_path: Path, ts_start: float, ts_end: float) -> None:
    if not report_path.exists():
        print(f"[report] warn: {report_path} not produced, skipping injection")
        return
    html_text = report_path.read_text()

    header = _build_header_panel(ts_start, ts_end)
    coverage_panel = _build_coverage_panel()
    corpus_panel = _build_corpus_panel()

    # pytest-html produces <body><h1 id="title">…</h1><p>Report generated…</p><h2>Environment</h2>…
    # Safest injection point: right after opening <body>.
    if "<body>" in html_text:
        html_text = html_text.replace("<body>", "<body>\n" + header, 1)
    else:
        html_text = header + html_text

    # Append coverage + corpus at the bottom, before </body>.
    tail = coverage_panel + corpus_panel
    if "</body>" in html_text:
        html_text = html_text.replace("</body>", tail + "</body>", 1)
    else:
        html_text += tail

    report_path.write_text(html_text)
    print(f"[report] enriched {report_path} (+ header/coverage/corpus panels)")


def main() -> int:
    ap = argparse.ArgumentParser(allow_abbrev=False)
    ap.add_argument("--skip-tests", action="store_true",
                    help="Skip pytest run; re-inject panels into the existing report.")
    ap.add_argument("pytest_args", nargs=argparse.REMAINDER,
                    help="Forward extra args after `--` to pytest")
    args = ap.parse_args()

    pytest_args: list[str] = []
    if args.pytest_args and args.pytest_args[0] == "--":
        pytest_args = args.pytest_args[1:]
    elif args.pytest_args:
        pytest_args = args.pytest_args

    ts_start = time.time()
    rc = 0
    if not args.skip_tests:
        rc = run_pytest(pytest_args)
    ts_end = time.time()

    inject_panels(REPORT_PATH, ts_start, ts_end)
    print(f"\nOpen: {REPORT_PATH}")
    return rc


if __name__ == "__main__":
    sys.exit(main())
