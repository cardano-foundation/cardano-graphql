
CREATE VIEW "AdaPots" AS
  SELECT
    epoch_no AS "epochNo",
    deposits,
    fees,
    reserves,
    rewards,
    slot_no AS "slotNo",
    treasury,
    utxo
FROM ada_pots;

CREATE TABLE IF NOT EXISTS "Asset" (
    "assetId" BYTEA PRIMARY KEY,
    "assetName" BYTEA,
    "decimals" INT,
    "description" VARCHAR(500),
    "fingerprint" CHAR(44),
    "firstAppearedInSlot" INT,
    "logo" VARCHAR(65536),
    "metadataHash" CHAR(40),
    "name" VARCHAR(50),
    "policyId" BYTEA,
    "ticker" VARCHAR(5),
    "url" VARCHAR(250)
);

CREATE VIEW "Block" AS
 SELECT (COALESCE(( SELECT sum((tx.fee)::bigint) AS sum
           FROM tx
          WHERE (tx.block_id = block.id)), (0)::NUMERIC))::bigint AS fees,
    block.hash,
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
 
CREATE VIEW "Collateral" AS
SELECT
  source_tx_out.address,
  source_tx_out.value,
  tx.hash AS "txHash",
  source_tx.hash AS "sourceTxHash",
  collateral_tx_in.tx_out_index AS "sourceTxIndex",
  source_tx_out.id AS source_tx_out_id
FROM
  tx
JOIN collateral_tx_in
  ON collateral_tx_in.tx_in_id = tx.id
JOIN tx_out AS source_tx_out
  ON collateral_tx_in.tx_out_id = source_tx_out.tx_id
  AND collateral_tx_in.tx_out_index = source_tx_out.index
JOIN tx AS source_tx
  ON source_tx_out.tx_id = source_tx.id;

CREATE VIEW "Delegation" AS
SELECT
  delegation.id AS "id",
  stake_address.view AS "address",
  delegation.redeemer_id AS "redeemerId",
  delegation.tx_id AS "tx_id",
  pool_hash_id AS "pool_hash_id"
FROM delegation
JOIN stake_address on delegation.addr_id = stake_address.id;

CREATE VIEW "Epoch" AS
SELECT
  epoch.fees AS "fees",
  epoch.out_sum AS "output",
  epoch.no AS "number",
  epoch_param.nonce AS "nonce",
  epoch.tx_count AS "transactionsCount",
  epoch.start_time AS "startedAt",
  epoch.end_time AS "lastBlockTime",
  epoch.blk_count AS "blocksCount"
FROM epoch
  LEFT JOIN epoch_param on epoch.no = epoch_param.epoch_no;

CREATE VIEW "ShelleyEpochProtocolParams" AS
SELECT
  epoch_param.influence AS "a0",
  epoch_param.coins_per_utxo_word AS "coinsPerUtxoWord",
  epoch_param.collateral_percent AS "collateralPercent",
  epoch_param.cost_models AS "costModels",
  epoch_param.decentralisation AS "decentralisationParam",
  epoch_param.max_collateral_inputs AS "maxCollateralInputs",
  epoch_param.max_epoch AS "eMax",
  epoch_param.epoch_no AS "epoch_no",
  epoch_param.entropy AS "extraEntropy",
  epoch_param.key_deposit AS "keyDeposit",
  epoch_param.max_block_size AS "maxBlockBodySize",
  epoch_param.max_block_ex_mem AS "maxBlockExMem",
  epoch_param.max_block_ex_steps AS "maxBlockExSteps",
  epoch_param.max_bh_size AS "maxBlockHeaderSize",
  epoch_param.max_tx_ex_mem AS "maxTxExMem",
  epoch_param.max_tx_ex_steps AS "maxTxExSteps",
  epoch_param.max_tx_size AS "maxTxSize",
  epoch_param.max_val_size AS "maxValSize",
  epoch_param.min_fee_a AS "minFeeA",
  epoch_param.min_fee_b AS "minFeeB",
  epoch_param.min_pool_cost AS "minPoolCost",
  epoch_param.min_utxo_value AS "minUTxOValue",
  epoch_param.optimal_pool_count AS "nOpt",
  epoch_param.pool_deposit AS "poolDeposit",
  epoch_param.price_mem AS "priceMem",
  epoch_param.price_step AS "priceStep",
  jsonb_build_object('major', epoch_param.protocol_major, 'minor', epoch_param.protocol_major) AS "protocolVersion",
  epoch_param.monetary_expand_rate AS "rho",
  epoch_param.treasury_growth_rate AS "tau"
FROM epoch_param;

CREATE VIEW "Redeemer" AS
SELECT
  redeemer.fee AS "fee",
  redeemer.id AS "id",
  redeemer.index AS "index",
  redeemer.purpose AS "purpose",
  redeemer.script_hash AS "scriptHash",
  redeemer.tx_id AS "txId",
  redeemer.unit_mem AS "unitMem",
  redeemer.unit_steps AS "unitSteps"
FROM redeemer;

CREATE VIEW "Reward" AS
SELECT
  reward.amount,
  stake_address.view AS "address",
  reward.earned_epoch AS "earnedInEpochNo",
  reward.pool_id AS pool_hash_id,
  reward.spendable_epoch AS "receivedInEpochNo",
  reward.type AS "type"
FROM reward
JOIN stake_address on reward.addr_id = stake_address.id;

CREATE VIEW "Script" AS
SELECT
  script.hash AS "hash",
  script.serialised_size AS "serialisedSize",
  script.type AS "type",
  script.tx_id AS "txId"
FROM script;

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
  stake_address.view AS "address",
  stake_deregistration.redeemer_id AS "redeemerId",
  stake_deregistration.tx_id AS "tx_id"
FROM stake_deregistration
JOIN stake_address on stake_deregistration.addr_id = stake_address.id;

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
  pool_hash.hash_raw AS "hash",
  pool_hash.view AS "id",
  pool.hash_id AS "hash_id",
  pool.id AS "update_id",
  pool.margin AS "margin",
  pool_metadata_ref.hash AS "metadataHash",
  block.block_no AS "blockNo",
  pool.registered_tx_id AS "updated_in_tx_id",
  pool.pledge AS "pledge",
  stake_address.view AS "rewardAddress",
  pool_metadata_ref.url AS "url"
FROM pool_update AS pool
  LEFT JOIN pool_metadata_ref ON pool.meta_id = pool_metadata_ref.id
  INNER JOIN tx ON pool.registered_tx_id = tx.id
  INNER JOIN latest_block_times ON latest_block_times.hash_id = pool.hash_id
  INNER JOIN block ON tx.block_id = block.id AND latest_block_times.blockTime = block.time
  JOIN stake_address on pool.reward_addr = stake_address.hash_raw
  JOIN pool_hash on pool_hash.id = pool.hash_id;

CREATE VIEW "StakePoolOwner" AS
SELECT
  stake_address.hash_raw as "hash",
  pool_owner.pool_hash_id as "pool_hash_id"
FROM pool_owner
  LEFT JOIN stake_address ON pool_owner.addr_id = stake_address.id;

CREATE VIEW "StakePoolRetirement" AS
SELECT
  retiring_epoch as "inEffectFrom",
  announced_tx_id as "tx_id",
  hash_id AS "pool_hash_id"
FROM pool_retire;

CREATE VIEW "StakeRegistration" AS
SELECT
  stake_registration.id AS "id",
  stake_address.view AS "address",
  stake_registration.tx_id AS "tx_id"
FROM stake_registration
JOIN stake_address on stake_registration.addr_id = stake_address.id;

CREATE VIEW "ActiveStake" AS
SELECT
  stake_address.view AS "address",
  amount AS "amount",
  epoch_no as "epochNo",
  epoch_stake.id AS "id",
  pool_hash.hash_raw AS "stakePoolHash",
  pool_hash.view AS "stakePoolId"
FROM epoch_stake
JOIN pool_hash
  ON pool_hash.id = epoch_stake.pool_id
JOIN stake_address on epoch_stake.addr_id = stake_address.id;

CREATE VIEW "TokenMint" AS
SELECT
  CAST(CONCAT(policy, RIGHT(CONCAT(E'\\', name), -3)) as BYTEA) as "assetId",
  name AS "assetName",
  policy AS "policyId",
  quantity,
  tx_id
FROM ma_tx_mint;

CREATE VIEW "TokenInOutput" AS
SELECT
  CAST(CONCAT(policy, RIGHT(CONCAT(E'\\', name), -3)) as BYTEA) as "assetId",
  name as "assetName",
  policy AS "policyId",
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
  tx.script_size AS "scriptSize",
  tx.size,
  CAST(COALESCE((SELECT SUM("value") FROM tx_out WHERE tx_id = tx.id), 0) AS bigint) AS "totalOutput",
  tx.valid_contract AS "validContract"
FROM
  tx
INNER JOIN block
  ON block.id = tx.block_id;

CREATE VIEW "TransactionInput" AS
SELECT
  source_tx_out.address,
  tx_in.redeemer_id AS "redeemerId",
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
  tx_out.address_has_script AS "addressHasScript",
  value,
  tx.hash AS "txHash",
  tx_out.id,
  index
FROM tx
JOIN tx_out
  ON tx.id = tx_out.tx_id;

CREATE VIEW "Utxo" AS SELECT
  address,
  tx_out.address_has_script AS "addressHasScript",
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
  stake_address.view "address",
  withdrawal.redeemer_id AS "redeemerId",
  withdrawal.tx_id AS "tx_id"
FROM withdrawal
JOIN stake_address on withdrawal.addr_id = stake_address.id;

CREATE INDEX idx_block_hash
    ON block(hash);

CREATE INDEX idx_ma_tx_mint_name
    ON ma_tx_mint(name);

CREATE INDEX idx_ma_tx_mint_policy
    ON ma_tx_mint(policy);

CREATE INDEX idx_reward_type
    ON reward(type);

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
    "TransactionOutput"."addressHasScript",
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

