#!/usr/bin/env python3
"""
Enforce Phase 2 performance baselines (task.md):

  p95 < 500 ms
  error rate < 1 %

against a Locust stats CSV (<prefix>_stats.csv written by `--csv=<prefix>`).

Usage:
  uv run python performance/check_baselines.py reports/perf-smoke-20260423T2100_stats.csv
  uv run python performance/check_baselines.py <prefix> --p95=500 --err-rate=1.0

Exit codes:
  0   all endpoints within baseline
  1   one or more endpoints breached
  2   bad input (missing file, parse error)
"""

from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path


def _resolve_csv(arg: str) -> Path:
    p = Path(arg)
    if p.is_file():
        return p
    candidate = p.parent / f"{p.name}_stats.csv"
    if candidate.is_file():
        return candidate
    raise FileNotFoundError(
        f"no stats file at {p} or {candidate}. Pass the full *_stats.csv path or the --csv= prefix you gave locust."
    )


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("csv", help="Path to *_stats.csv or the --csv prefix")
    ap.add_argument("--p95", type=float, default=500.0, help="p95 latency ceiling in milliseconds")
    ap.add_argument("--err-rate", type=float, default=1.0, help="error-rate ceiling in percent")
    ap.add_argument("--ignore-aggregated", action="store_true",
                    help="skip the 'Aggregated' row (evaluate per-endpoint only)")
    args = ap.parse_args()

    try:
        path = _resolve_csv(args.csv)
    except FileNotFoundError as e:
        print(f"error: {e}", file=sys.stderr)
        return 2

    breaches: list[str] = []
    rows_evaluated = 0

    with open(path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row.get("Name") or ""
            if args.ignore_aggregated and name == "Aggregated":
                continue
            requests = int(row.get("Request Count") or 0)
            if requests == 0:
                # No traffic → can't judge; skip quietly.
                continue

            fails = int(row.get("Failure Count") or 0)
            err_rate = 100.0 * fails / requests
            # Locust CSV columns: 95% is "95%" header (percentile)
            p95_str = row.get("95%") or row.get("95%ile (ms)") or "0"
            p95 = float(p95_str)

            status = "OK"
            if p95 > args.p95:
                breaches.append(
                    f"{name}: p95={p95:.0f}ms > {args.p95:.0f}ms ceiling"
                )
                status = "BREACH"
            if err_rate > args.err_rate:
                breaches.append(
                    f"{name}: error_rate={err_rate:.2f}% > {args.err_rate:.2f}% ceiling"
                )
                status = "BREACH"

            rows_evaluated += 1
            print(
                f"[{status}] {name:<32}  n={requests:<5}  p95={p95:>5.0f}ms  "
                f"fail={fails} ({err_rate:.2f}%)"
            )

    if rows_evaluated == 0:
        print("error: no rows with traffic to evaluate", file=sys.stderr)
        return 2

    if breaches:
        print("\nBaseline breaches:")
        for b in breaches:
            print(f"  - {b}")
        return 1

    print("\nAll endpoints within baseline.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
