
CREATE TABLE IF NOT EXISTS "Asset"
  AS
  SELECT
    DISTINCT CONCAT(RIGHT(CONCAT(E'\\', policy), -3), RIGHT(CONCAT(E'\\', name), -3)) as "assetId",
    RIGHT(CONCAT(E'\\', name), -3) as "assetName",
    CAST(NULL AS TEXT) AS "description",
    CAST(NULL AS CHAR(44)) as "fingerprint",
    CAST(NULL AS TEXT) AS "logo",
    0 AS "metadataFetchAttempts",
    CAST(NULL AS CHAR(40)) AS "metadataHash",
    CAST(NULL AS TEXT) AS "name",
    policy as "policyId",
    CAST(NULL AS TEXT) AS "ticker",
    CAST(NULL AS TEXT) AS "url"
  FROM ma_tx_out;

ALTER TABLE "Asset" ADD PRIMARY KEY ("assetId");

CREATE VIEW "Block" AS
 SELECT (COALESCE(( SELECT sum((tx.fee)::bigint) AS sum
           FROM tx
          WHERE (tx.block_id = block.id)), (0)::NUMERIC))::bigint AS fees,
    block.hash,
    block.merkle_root AS "merkleRoot",
    block.block_no AS "number",
    block.op_cert AS "opCert",
    previous_block.hash AS "previousBlockHash",
    next_block.hash AS "nextBlockHash",
    jsonb_build_object('major', block.proto_major, 'minor', block.proto_minor) AS "protocolVersion",
    block.size,
    block.tx_count AS "transactionsCount",
    block.epoch_no AS "epochNo",
    block."time" AS "forgedAt",
    block.epoch_slot_no AS "slotInEpoch",
    block.slot_no AS "slotNo",
    slot_leader.id AS "slot_leader_id",
    slot_leader.pool_hash_id AS "pool_hash_id",
    block.vrf_key As "vrfKey"
   FROM (((block
     LEFT JOIN block previous_block ON ((block.previous_id = previous_block.id)))
     LEFT JOIN block next_block ON ((next_block.previous_id = block.id)))
     LEFT JOIN slot_leader ON ((block.slot_leader_id = slot_leader.id)));

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
  pool_hash_id AS "pool_hash_id"
FROM delegation;

CREATE VIEW "Epoch" AS
SELECT
  epoch.fees AS "fees",
  epoch.out_sum AS "output",
  epoch.no AS "number",
  (
  	SELECT epoch_param.nonce
  	FROM epoch_param
  	WHERE epoch_param.epoch_no = epoch.no
  ) AS "nonce",
  epoch.tx_count AS "transactionsCount",
  epoch.start_time AS "startedAt",
  epoch.end_time AS "lastBlockTime",
  epoch.blk_count AS "blocksCount"
FROM epoch;

CREATE VIEW "ShelleyEpochProtocolParams" AS
SELECT
  epoch_param.influence AS "a0",
  epoch_param.decentralisation AS "decentralisationParam",
  epoch_param.max_epoch AS "eMax",
  epoch_param.epoch_no AS "epoch_no",
  epoch_param.entropy AS "extraEntropy",
  epoch_param.key_deposit AS "keyDeposit",
  epoch_param.max_block_size AS "maxBlockBodySize",
  epoch_param.max_bh_size AS "maxBlockHeaderSize",
  epoch_param.max_tx_size AS "maxTxSize",
  epoch_param.min_fee_a AS "minFeeA",
  epoch_param.min_fee_b AS "minFeeB",
  epoch_param.min_pool_cost AS "minPoolCost",
  epoch_param.min_utxo_value AS "minUTxOValue",
  epoch_param.optimal_pool_count AS "nOpt",
  epoch_param.pool_deposit AS "poolDeposit",
  jsonb_build_object('major', epoch_param.protocol_major, 'minor', epoch_param.protocol_major) AS "protocolVersion",
  epoch_param.monetary_expand_rate AS "rho",
  epoch_param.treasury_growth_rate AS "tau"
FROM epoch_param;

CREATE VIEW "Reward" AS
SELECT
  reward.amount AS "amount",
  (
	  SELECT stake_address.view
	  FROM stake_address
	  WHERE stake_address.id = reward.addr_id
  ) AS "address",
  reward.epoch_no AS "epochNo",
  reward.pool_id AS "pool_hash_id"
FROM reward;

CREATE VIEW "SlotLeader" AS
SELECT
  slot_leader.hash AS "hash",
  slot_leader.id AS "id",
  slot_leader.description AS "description",
  slot_leader.pool_hash_id AS "pool_hash_id"
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
    JOIN block ON tx.block_id = block.id
    group by pool.hash_id
)
SELECT
  pool.fixed_cost AS "fixedCost",
  ( SELECT pool_hash.hash_raw FROM pool_hash WHERE pool_hash.id = pool.hash_id ) AS "hash",
  ( SELECT pool_hash.view FROM pool_hash WHERE pool_hash.id = pool.hash_id ) AS "id",
  pool.hash_id AS "hash_id",
  pool.id AS "update_id",
  pool.margin AS "margin",
  pool_meta_data.hash AS "metadataHash",
  block.block_no AS "blockNo",
  pool.registered_tx_id AS "updated_in_tx_id",
  pool.pledge AS "pledge",
  ( SELECT stake_address.view FROM stake_address WHERE stake_address.hash_raw = pool.reward_addr) AS "rewardAddress",
  pool_meta_data.url AS "url"
FROM pool_update AS pool
  LEFT JOIN pool_meta_data ON pool.meta_id = pool_meta_data.id
  INNER JOIN tx ON pool.registered_tx_id = tx.id
  INNER JOIN latest_block_times ON latest_block_times.hash_id = pool.hash_id
  INNER JOIN block ON tx.block_id = block.id AND latest_block_times.blockTime = block.time;

CREATE VIEW "StakePoolRetirement" AS
SELECT
  retiring_epoch as "inEffectFrom",
  announced_tx_id as "tx_id",
  hash_id AS "pool_hash_id"
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

CREATE VIEW "ActiveStake" AS
SELECT
  (
  	SELECT stake_address.view
  	FROM stake_address
  	WHERE stake_address.id = epoch_stake.addr_id
  ) AS "address",
  amount AS "amount",
  epoch_no as "epochNo",
  epoch_stake.id AS "id",
  pool_hash.hash_raw AS "stakePoolHash",
  pool_hash.view AS "stakePoolId"
FROM epoch_stake
JOIN pool_hash
  ON pool_hash.id = epoch_stake.pool_id;

CREATE VIEW "Mint" AS
SELECT
  CONCAT(RIGHT(CONCAT(E'\\', policy), -3), RIGHT(CONCAT(E'\\', name), -3)) as "assetId",
  quantity,
  tx_id
FROM ma_tx_mint;

CREATE VIEW "Token" AS
SELECT
  CONCAT(RIGHT(CONCAT(E'\\',policy), -3), RIGHT(CONCAT(E'\\',name), -3)) as "assetId",
  RIGHT(CONCAT(E'\\', name), -3) as "assetName",
  policy as "policyId",
  quantity,
  tx_out_id
FROM ma_tx_out;

CREATE VIEW "Transaction" AS
SELECT
  block.hash AS "blockHash",
  tx.block_index AS "blockIndex",
  tx.deposit AS "deposit",
  COALESCE(tx.fee, 0) AS fee,
  tx.hash,
  tx.id,
  block.time AS "includedAt",
  tx.invalid_before AS "invalidBefore",
  tx.invalid_hereafter AS "invalidHereafter",
  tx.size,
  CAST(COALESCE((SELECT SUM("value") FROM tx_out WHERE tx_id = tx.id), 0) AS bigint) AS "totalOutput"
FROM
  tx
INNER JOIN block
  ON block.id = tx.block_id;

CREATE VIEW "TransactionInput" AS
SELECT
  source_tx_out.address,
  source_tx_out.value,
  tx.hash AS "txHash",
  source_tx.hash AS "sourceTxHash",
  tx_in.tx_out_index AS "sourceTxIndex",
  source_tx_out.id AS source_tx_out_id
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
  tx_out.id,
  index
FROM tx
JOIN tx_out
  ON tx.id = tx_out.tx_id;

CREATE VIEW "Utxo" AS SELECT
  address,
  value,
  tx.hash AS "txHash",
  tx_out.id,
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
    "TransactionOutput"."id",
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
  AND tx.block_id <= (SELECT id FROM block WHERE hash = "hash")
$$ LANGUAGE SQL stable;

