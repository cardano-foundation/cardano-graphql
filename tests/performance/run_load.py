#!/usr/bin/env python3
"""
Thin CLI wrapper around `locust` so load tests are reproducible.

Profiles:
  smoke      5 users, 60s   — CI sanity, verifies plumbing
  soak       20 users, 30m  — steady-state stability
  stability  50 users, 5m   — capacity / percentile probe

Examples:
  uv run python performance/run_load.py --profile=smoke
  uv run python performance/run_load.py --profile=soak --url=http://localhost:3100
  uv run python performance/run_load.py --profile=stability --users=100 --run-time=10m
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.resolve()))

from performance._config import load_network  # noqa: E402

TESTS_DIR = Path(__file__).parent.parent.resolve()
LOCUSTFILE = TESTS_DIR / "performance" / "locustfile.py"
REPORTS_DIR = TESTS_DIR / "reports"

PROFILES = {
    # name:        (users, spawn_rate, run_time)
    "smoke":      (5,   1, "60s"),
    "soak":       (20,  2, "30m"),
    "stability":  (50,  5, "5m"),
}


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--profile", choices=PROFILES.keys(), default="smoke")
    p.add_argument("--url", default=None, help="override network_config URL")
    p.add_argument("--users", type=int, default=None, help="override profile users")
    p.add_argument("--spawn-rate", type=float, default=None)
    p.add_argument("--run-time", default=None, help="e.g. 60s, 5m, 1h")
    p.add_argument("--csv-prefix", default=None)
    p.add_argument("--html", default=None, help="path to HTML report (optional)")
    p.add_argument("--load-profile", default="full", choices=["full", "light"],
                   help="full includes heavy paymentAddresses/transactionsInBlock tasks; light skips them")
    args = p.parse_args()

    users, spawn, run_time = PROFILES[args.profile]
    if args.users is not None:
        users = args.users
    if args.spawn_rate is not None:
        spawn = args.spawn_rate
    if args.run_time is not None:
        run_time = args.run_time

    net = load_network()
    # Precedence: --url > GRAPHQL_URL env > networks.yaml url
    host = args.url or os.environ.get("GRAPHQL_URL") or net["url"]

    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    ts = time.strftime("%Y%m%dT%H%M%S")
    prefix = args.csv_prefix or str(REPORTS_DIR / f"perf-{args.profile}-{ts}")

    env = dict(os.environ)
    env["LOAD_PROFILE"] = args.load_profile

    cmd = [
        "locust",
        "-f", str(LOCUSTFILE),
        "--host", host,
        "--headless",
        "-u", str(users),
        "-r", str(spawn),
        "-t", run_time,
        "--csv", prefix,
        "--csv-full-history",
        "--only-summary",
    ]
    if args.html:
        cmd += ["--html", args.html]

    print(
        f"[run_load] profile={args.profile} users={users} spawn={spawn} "
        f"run_time={run_time} host={host} load_profile={args.load_profile}"
    )
    print("[run_load] csv prefix:", prefix)

    return subprocess.call(cmd, env=env)


if __name__ == "__main__":
    sys.exit(main())
