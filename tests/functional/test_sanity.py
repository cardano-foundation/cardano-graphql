"""
Sanity tests — run first, block the suite if the endpoint is not ready.

The readiness fixture (conftest.py::sync_readiness) already polls cardanoDbMeta
until initialized + syncPercentage>=99. These tests then assert on
visible invariants (tip >= anchor, genesis networkMagic matches config).
"""

import pytest


pytestmark = [pytest.mark.sanity, pytest.mark.pr]


def test_cardano_db_meta_is_initialized(client):
    resp = client.execute("{ cardanoDbMeta { initialized syncPercentage } }")
    assert resp.status_code == 200
    assert not resp.errors, f"unexpected GraphQL errors: {resp.errors}"
    meta = resp.data["cardanoDbMeta"]
    assert meta["initialized"] is True
    assert float(meta["syncPercentage"]) >= 99.0, (
        f"syncPercentage too low: {meta['syncPercentage']}"
    )


def test_chain_tip_is_past_anchor_block(client, anchor_block):
    resp = client.execute("{ cardano { tip { number hash epoch { number } } } }")
    assert not resp.errors, f"unexpected GraphQL errors: {resp.errors}"
    tip = resp.data["cardano"]["tip"]
    assert tip["number"] >= anchor_block, (
        f"tip {tip['number']} < anchor {anchor_block}; anchor may be too recent"
    )
    assert len(tip["hash"]) == 64
    assert tip["epoch"]["number"] >= 0


def test_genesis_network_magic_matches_config(client, network_config):
    resp = client.execute(
        "{ genesis { shelley { networkMagic } } }"
    )
    assert not resp.errors, f"unexpected GraphQL errors: {resp.errors}"
    magic = resp.data["genesis"]["shelley"]["networkMagic"]
    expected = network_config["network_magic"]
    assert int(magic) == int(expected), (
        f"networkMagic mismatch: got {magic}, expected {expected}"
    )


def test_anchor_block_is_fetchable(client, anchor_block):
    """Prove that the pinned anchor block is still indexed and returnable."""
    resp = client.execute(
        "query($n: Int!) { blocks(where: { number: { _eq: $n } }) "
        "{ hash number epochNo size } }",
        variables={"n": anchor_block},
    )
    assert not resp.errors, f"unexpected GraphQL errors: {resp.errors}"
    blocks = resp.data["blocks"]
    assert len(blocks) == 1, f"expected exactly 1 block at {anchor_block}, got {len(blocks)}"
    block = blocks[0]
    assert block["number"] == anchor_block
    assert len(block["hash"]) == 64
    assert block["size"] > 0
