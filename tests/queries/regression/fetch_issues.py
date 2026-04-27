#!/usr/bin/env python3
"""
Fetch closed bug issues from cardano-foundation/cardano-graphql and emit
skeleton .graphql + .json stubs for each one — you still need to write the
repro query and capture the golden manually.

Uses unauthenticated GitHub REST API (60 req/h limit). For higher limits,
set GITHUB_TOKEN.

Usage:
  uv run python queries/regression/fetch_issues.py --limit 20
  uv run python queries/regression/fetch_issues.py --limit 10 --labels bug
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import urllib.request
from pathlib import Path

REPO = "cardano-foundation/cardano-graphql"
TESTS_DIR = Path(__file__).parent.parent.parent.resolve()
QUERIES_DIR = TESTS_DIR / "queries" / "regression"
GOLDEN_DIR = TESTS_DIR / "golden" / "functional" / "regression"


def slugify(title: str, max_len: int = 40) -> str:
    s = re.sub(r"[^a-zA-Z0-9]+", "_", title.lower()).strip("_")
    return s[:max_len]


def fetch(path: str) -> list[dict]:
    url = f"https://api.github.com/repos/{REPO}/{path}"
    req = urllib.request.Request(url, headers={"Accept": "application/vnd.github+json"})
    tok = os.environ.get("GITHUB_TOKEN")
    if tok:
        req.add_header("Authorization", f"Bearer {tok}")
    with urllib.request.urlopen(req, timeout=30) as r:
        if r.getheader("X-RateLimit-Remaining") == "0":
            print("warn: github rate limit reached", file=sys.stderr)
        return json.loads(r.read())


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--limit", type=int, default=10)
    ap.add_argument("--labels", default=None, help="comma-separated labels filter (e.g. 'bug')")
    ap.add_argument("--anchor-block", type=int, default=13228000)
    args = ap.parse_args()

    q = f"issues?state=closed&per_page={min(args.limit, 100)}"
    if args.labels:
        q += f"&labels={args.labels}"

    issues = fetch(q)
    issues = [i for i in issues if "pull_request" not in i][: args.limit]

    QUERIES_DIR.mkdir(parents=True, exist_ok=True)
    GOLDEN_DIR.mkdir(parents=True, exist_ok=True)

    created = 0
    for issue in issues:
        n = issue["number"]
        title = issue.get("title") or f"issue_{n}"
        slug = slugify(title)

        q_path = QUERIES_DIR / f"issue_{n}_{slug}.graphql"
        g_path = GOLDEN_DIR / f"issue_{n}_{slug}.json"
        if q_path.exists() or g_path.exists():
            print(f"skip: #{n} already has artefacts")
            continue

        with open(q_path, "w") as f:
            f.write(
                f"# TODO: write the repro query for cardano-graphql#{n}\n"
                f"# Title: {title}\n"
                f"# URL:   {issue.get('html_url')}\n"
                "query placeholder { cardano { tip { number } } }\n"
            )
        golden = {
            "test_name": f"issue_{n}_{slug}",
            "description": (
                f"Regression for cardano-foundation/cardano-graphql#{n} — "
                f"{title} (placeholder; write a real repro query)."
            ),
            "network": "mainnet",
            "query_file": f"queries/regression/issue_{n}_{slug}.graphql",
            "variables": {},
            "anchor_block": args.anchor_block,
            "ignore_paths": [],
            "graphql_errors_allowed": False,
            "regression_issue": n,
        }
        with open(g_path, "w") as f:
            json.dump(golden, f, indent=2, sort_keys=True)
            f.write("\n")
        created += 1
        print(f"seeded: #{n:<4} {q_path.name}")

    print(f"\n{created} new stub(s). Next:")
    print("  1. replace placeholder queries with real repros")
    print("  2. refine ignore_paths as needed")
    print("  3. uv run pytest -k 'regression and issue_<N>' --update-golden")
    return 0


if __name__ == "__main__":
    sys.exit(main())
