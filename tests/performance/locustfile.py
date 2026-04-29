"""
Locust user for cardano-graphql.

Each @task is a named endpoint in Locust stats output. Weights approximate a
realistic production traffic mix — cheap lookups dominate, heavy aggregate /
historical-scan queries are rare.

Run via `performance/run_load.py` (preferred) or directly:
  uv run locust -f performance/locustfile.py --host=http://localhost:3100 \
    --headless -u 10 -r 2 -t 60s --csv=reports/perf-smoke

Environment:
  CARDANO_NETWORK     switches network_config (default: mainnet)
  LOAD_PROFILE        light|full (default: full) — light drops heavy queries
"""

from __future__ import annotations

import json
import os
import random
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.resolve()))

from locust import HttpUser, between, task  # noqa: E402

from performance._config import load_network, load_query_text  # noqa: E402

NETWORK = load_network()
ANCHOR = NETWORK["anchor_block"]
ADDRS = NETWORK.get("test_addresses") or {}
ADDR_LIGHT = ADDRS.get("light")
ADDR_MEDIUM = ADDRS.get("medium")
ADDR_HEAVY = ADDRS.get("heavy")
STAKE_POOL_IDS = [p["id"] for p in (NETWORK.get("test_stake_pools") or [])]
TX_HASHES = list(NETWORK.get("test_transactions_at_anchor") or [])
ASSET_IDS = list(NETWORK.get("test_asset_ids") or [])

PROFILE = os.environ.get("LOAD_PROFILE", "full").lower()


def _graphql(l: "GraphQLUser", name: str, query: str, variables: dict | None = None):
    body = {"query": query, "variables": variables or {}}
    with l.client.post(
        "/graphql",
        json=body,
        name=name,
        catch_response=True,
        headers={"Content-Type": "application/json"},
    ) as resp:
        if resp.status_code != 200:
            resp.failure(f"HTTP {resp.status_code}: {resp.text[:200]}")
            return
        try:
            payload = resp.json()
        except Exception as e:  # noqa: BLE001
            resp.failure(f"bad JSON: {e}")
            return
        if payload.get("errors"):
            resp.failure(
                "graphql errors: "
                + json.dumps([e.get("message") for e in payload["errors"]])[:300]
            )
            return
        resp.success()


class GraphQLUser(HttpUser):
    wait_time = between(0.5, 2.0)

    # Lightweight queries — weighted high
    @task(25)
    def cardano_tip(self):
        q = load_query_text("queries/fireblocks/cardanoTip.graphql")
        _graphql(self, "cardanoTip", q)

    @task(20)
    def block_by_number(self):
        q = load_query_text("queries/fireblocks/blockByNumber.graphql")
        _graphql(self, "blockByNumber", q, {"number": ANCHOR})

    @task(15)
    def cardano_db_sync_progress(self):
        q = load_query_text("queries/example/meta/cardanoDbSyncProgress.graphql")
        _graphql(self, "cardanoDbSyncProgress", q)

    @task(10)
    def stake_pool_by_id(self):
        if not STAKE_POOL_IDS:
            return
        q = load_query_text("queries/example/stake_pools/stakePoolById.graphql")
        _graphql(self, "stakePoolById", q, {"id": random.choice(STAKE_POOL_IDS)})

    @task(10)
    def transaction_by_hash(self):
        if not TX_HASHES:
            return
        q = load_query_text("queries/fireblocks/transactionByHash.graphql")
        _graphql(self, "transactionByHash", q, {"hash": random.choice(TX_HASHES)})

    # Medium-cost — traversal / aggregates
    @task(5)
    def great_grandchild_block(self):
        q = load_query_text(
            "queries/example/blocks/selectGreatGrandchildBlock.graphql"
        )
        _graphql(self, "selectGreatGrandchildBlock", q, {"number": ANCHOR})

    @task(5)
    def epoch_details_finalized(self):
        q = load_query_text(
            "queries/example/epochs/epochDetailsByNumber.graphql"
        )
        finalized = (NETWORK.get("test_epochs") or {}).get("finalized")
        if finalized is None:
            return
        _graphql(self, "epochDetailsByNumber", q, {"number": finalized})

    @task(3)
    def assets_first3(self):
        if len(ASSET_IDS) < 1:
            return
        q = load_query_text("queries/example/assets/assets.graphql")
        _graphql(
            self,
            "assets_first3",
            q,
            {
                "limit": 3,
                "offset": 0,
                "where": {"assetId": {"_in": ASSET_IDS}},
            },
        )

    # Heavy queries — skipped in the light profile; rare otherwise
    @task(1)
    def transactions_in_block(self):
        if PROFILE == "light":
            return
        q = load_query_text("queries/fireblocks/transactionsInBlock.graphql")
        _graphql(self, "transactionsInBlock", q, {"blockNum": ANCHOR})

    # ----- Address-bucket tasks -----
    # Weighted by realistic traffic mix: cheap wallets dominate, DEX/contract
    # addresses are rarer but their tail latency matters. Each bucket is a
    # distinct endpoint in Locust stats so the baseline checker flags them
    # individually.

    @task(8)
    def payment_summary_light(self):
        if not ADDR_LIGHT:
            return
        q = load_query_text("queries/fireblocks/paymentAddressSummary.graphql")
        _graphql(
            self, "paymentAddressSummary_light", q,
            {"addresses": [ADDR_LIGHT], "atBlock": ANCHOR},
        )

    @task(3)
    def payment_summary_medium(self):
        if PROFILE == "light" or not ADDR_MEDIUM:
            return
        q = load_query_text("queries/fireblocks/paymentAddressSummary.graphql")
        _graphql(
            self, "paymentAddressSummary_medium", q,
            {"addresses": [ADDR_MEDIUM], "atBlock": ANCHOR},
        )

    @task(1)
    def payment_summary_heavy(self):
        if PROFILE == "light" or not ADDR_HEAVY:
            return
        q = load_query_text("queries/fireblocks/paymentAddressSummary.graphql")
        _graphql(
            self, "paymentAddressSummary_heavy", q,
            {"addresses": [ADDR_HEAVY], "atBlock": ANCHOR},
        )

    @task(4)
    def utxo_set_light(self):
        if not ADDR_LIGHT:
            return
        q = load_query_text("queries/fireblocks/utxoSetForAddresses.graphql")
        _graphql(
            self, "utxoSetForAddresses_light", q,
            {"addresses": [ADDR_LIGHT]},
        )

    @task(1)
    def utxo_set_heavy(self):
        if PROFILE == "light" or not ADDR_HEAVY:
            return
        q = load_query_text("queries/fireblocks/utxoSetForAddresses.graphql")
        _graphql(
            self, "utxoSetForAddresses_heavy", q,
            {"addresses": [ADDR_HEAVY]},
        )
