CREATE VIEW "Block" AS
 SELECT (COALESCE(( SELECT sum((tx.fee)::bigint) AS sum
           FROM tx
          WHERE (tx.block = block.id)), (0)::NUMERIC))::bigint AS fees,
    block.hash,
    block.merkel_root AS "merkelRoot",
    block.block_no AS "number",
    block.op_cert AS "opCert",
    previous_block.hash AS "previousBlockHash",
    next_block.hash AS "nextBlockHash",
    block.proto_version AS "protocolVersion",
    block.size,
    block.tx_count AS "transactionsCount",
    block.epoch_no AS "epochNo",
    block."time" AS "forgedAt",
    block.epoch_slot_no AS "slotInEpoch",
    block.slot_no AS "slotNo",
    slot_leader.id AS "slot_leader_id",
    block.vrf_key As "vrfKey"
   FROM (((block
     LEFT JOIN block previous_block ON ((block.previous = previous_block.id)))
     LEFT JOIN block next_block ON ((next_block.previous = block.id)))
     LEFT JOIN slot_leader ON ((block.slot_leader = slot_leader.id)));

CREATE OR REPLACE VIEW "Cardano" AS
 SELECT block.block_no AS "tipBlockNo",
    block.epoch_no AS "currentEpochNo"
   FROM block
  WHERE (block.block_no IS NOT NULL)
  ORDER BY block.block_no DESC
 LIMIT 1;
 
CREATE VIEW "Delegation" AS
SELECT
  delegation.id AS "id",
  (
	  SELECT stake_address.view
	  FROM stake_address
	  WHERE stake_address.id = delegation.addr_id
  ) AS "address",
  delegation.tx_id AS "tx_id",
  pool_hash.hash AS "pool_hash"
FROM
  delegation
LEFT OUTER JOIN pool_hash
  ON delegation.pool_hash_id = pool_hash.id;

CREATE VIEW "Epoch" AS
SELECT
  epoch.fees AS "fees",
  epoch.out_sum AS "output",
  epoch.no AS "number",
  epoch.tx_count AS "transactionsCount",
  epoch.start_time AS "startedAt",
  epoch.end_time AS "lastBlockTime",
  epoch.blk_count AS "blocksCount"
FROM epoch;

CREATE VIEW "Reward" AS
SELECT
  reward.amount AS "amount",
  reward.id AS "id",
  (
	  SELECT stake_address.view
	  FROM stake_address
	  WHERE stake_address.id = reward.addr_id
  ) AS "address",
  reward.tx_id AS "tx_id"
FROM reward;

CREATE VIEW "SlotLeader" AS
SELECT
  slot_leader.hash AS "hash",
  slot_leader.id AS "id",
  slot_leader.description AS "description",
  ( SELECT pool_hash.hash FROM pool_hash WHERE pool_hash.id = slot_leader.pool_hash_id ) AS "pool_hash"
FROM slot_leader;

CREATE VIEW "StakeDeregistration" AS
SELECT
  stake_deregistration.id AS "id",
  (
	  SELECT stake_address.view
	  FROM stake_address
	  WHERE stake_address.id = stake_deregistration.addr_id
  ) AS "address",
  stake_deregistration.tx_id AS "tx_id"
FROM stake_deregistration;

CREATE VIEW "StakePool" AS
WITH
  latest_block_times AS (
    SELECT pool.hash_id, max(block.time) AS blockTime
    FROM pool_update AS pool
    JOIN tx ON pool.registered_tx_id = tx.id
    JOIN block ON tx.block = block.id
    group by pool.hash_id
)
SELECT
  pool.fixed_cost AS "fixedCost",
  ( SELECT pool_hash.hash FROM pool_hash WHERE pool_hash.id = pool.hash_id ) AS "hash",
  pool.id AS "id",
  pool.margin AS "margin",
  pool_meta_data.hash AS "metadataHash",
  block.block_no AS "blockNo",
  pool.registered_tx_id AS "updated_in_tx_id",
  pool.pledge AS "pledge",
  ( SELECT stake_address.view FROM stake_address WHERE stake_address.id = pool.reward_addr_id) AS "rewardAddress",
  pool_meta_data.url AS "url"
FROM pool_update AS pool
  INNER JOIN pool_meta_data ON pool.meta = pool_meta_data.id
  INNER JOIN tx ON pool.registered_tx_id = tx.id
  INNER JOIN latest_block_times ON latest_block_times.hash_id = pool.hash_id
  INNER JOIN block ON tx.block = block.id AND latest_block_times.blockTime = block.time;

CREATE VIEW "StakePoolRetirement" AS
SELECT
  retiring_epoch as "inEffectFrom",
  announced_tx_id as "tx_id",
  ( SELECT pool_hash.hash FROM pool_hash WHERE pool_hash.id = hash_id ) AS "pool_hash"
FROM pool_retire;

CREATE VIEW "StakeRegistration" AS
SELECT
  stake_registration.id AS "id",
  (
	  SELECT stake_address.view
	  FROM stake_address
	  WHERE stake_address.id = stake_registration.addr_id
  ) AS "address",
  stake_registration.tx_id AS "tx_id"
FROM stake_registration;

CREATE VIEW "Transaction" AS
SELECT
  block.hash AS "blockHash",
  tx.block_index AS "blockIndex",
  tx.deposit AS "deposit",
  COALESCE(tx.fee, 0) AS fee,
  tx.hash,
  tx.id,
  block.time AS "includedAt",
  tx.size,
  CAST(COALESCE((SELECT SUM("value") FROM tx_out WHERE tx_id = tx.id), 0) AS bigint) AS "totalOutput"
FROM
  tx
INNER JOIN block
  ON block.id = tx.block;

CREATE VIEW "TransactionInput" AS
SELECT
  source_tx_out.address,
  source_tx_out.value,
  tx.hash AS "txHash",
  source_tx.hash AS "sourceTxHash",
  tx_in.tx_out_index AS "sourceTxIndex"
FROM
  tx
JOIN tx_in
  ON tx_in.tx_in_id = tx.id
JOIN tx_out AS source_tx_out
  ON tx_in.tx_out_id = source_tx_out.tx_id
  AND tx_in.tx_out_index = source_tx_out.index
JOIN tx AS source_tx
  ON source_tx_out.tx_id = source_tx.id;

CREATE VIEW "TransactionOutput" AS
SELECT
  address,
  value,
  tx.hash AS "txHash",
  index
FROM tx
JOIN tx_out
  ON tx.id = tx_out.tx_id;

CREATE VIEW "Utxo" AS SELECT
  address,
  value,
  tx.hash AS "txHash",
  index
FROM tx
JOIN tx_out
  ON tx.id = tx_out.tx_id
LEFT OUTER JOIN tx_in
  ON tx_out.tx_id = tx_in.tx_out_id
  AND tx_out.index = tx_in.tx_out_index
WHERE tx_in.tx_in_id IS NULL;

CREATE VIEW "Withdrawal" AS
SELECT
  withdrawal.amount AS "amount",
  withdrawal.id AS "id",
  (
	  SELECT stake_address.view
	  FROM stake_address
	  WHERE stake_address.id = withdrawal.addr_id
  ) AS "address",
  withdrawal.tx_id AS "tx_id"
FROM withdrawal;

CREATE INDEX idx_block_hash
    ON block(hash);

CREATE INDEX idx_tx_hash
    ON tx(hash);

 CREATE INDEX idx_tx_in_consuming_tx
    ON tx_in(tx_out_id);

CREATE INDEX idx_tx_out_tx
    ON tx_out(tx_id);

CREATE function utxo_set_at_block("hash" hash32type)
RETURNS SETOF "TransactionOutput" AS $$
  SELECT
    "TransactionOutput".address,
    "TransactionOutput".value,
    "TransactionOutput"."txHash",
    "TransactionOutput".index
  FROM tx
  JOIN tx_out
    ON tx.id = tx_out.tx_id
  JOIN "TransactionOutput"
    ON tx.hash = "TransactionOutput"."txHash"
  LEFT OUTER JOIN tx_in
    ON tx_out.tx_id = tx_in.tx_out_id
    AND tx_out.index = tx_in.tx_out_index
  WHERE tx_in.tx_in_id IS NULL
  AND tx.block <= (SELECT id FROM block WHERE hash = "hash")
$$ LANGUAGE SQL stable;

