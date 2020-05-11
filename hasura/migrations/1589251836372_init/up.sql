
create view "Block" as
select
  CAST(COALESCE((select sum(tx.fee) from tx where tx.block = block.id), 0) as integer) as "fees",
  block.hash as hash,
  block.merkel_root as "merkelRoot",
  block.block_no as number,
  previous_block."hash" as "previousBlockHash",
  next_block."hash" as "nextBlockHash",
  slot_leader."description" as "createdBy",
  block.size as size,
  block.tx_count as "transactionsCount",
  block.epoch_no as "epochNo",
  block.slot_no as "slotNo",
  block.slot_no - (block.epoch_no * (10 * (select protocol_const from meta))) as "slotWithinEpoch",
  block.time as "createdAt"
from block
left outer join block as previous_block
  on block.previous = previous_block.id
left outer join block as next_block
  on next_block.previous = block.id
left outer join slot_leader
  on block.slot_leader = slot_leader.id;

create view "Cardano" as
select
  number as "blockHeight",
  "epochNo" as "currentEpochNo",
  (select slot_duration from meta) as "slotDuration",
  (select start_time from meta) as "startTime",
  (select protocol_const from meta) as "protocolConst",
	(select network_name from meta) as "networkName"
from "Block"
where number is not null
order by number desc
limit 1;

create view "Epoch" as
select
  epoch.out_sum as "output",
  epoch.no as "number",
  epoch.tx_count as "transactionsCount",
  epoch.start_time as "startedAt",
  epoch.end_time as "lastBlockTime",
  epoch.blk_count as "blocksCount"
from epoch;

create view "Transaction" as
select
  block.hash as "blockHash",
  COALESCE(tx.fee, 0) as fee,
  tx.hash as hash,
  cast((select sum("value") from tx_out where tx_id = tx.id) as bigint) as "totalOutput",
	tx.size,
  block.time as "includedAt"
from
  tx
inner join block
  on block.id = tx.block;

create view "Utxo" as select
  address,
  value,
  tx.hash as "txHash",
  index
from tx
join tx_out
  on tx.id = tx_out.tx_id
left outer join tx_in
  on tx_out.tx_id = tx_in.tx_out_id
  and tx_out.index = tx_in.tx_out_index
where tx_in.tx_in_id is null;

create view "TransactionInput" as
select
  source_tx_out.address,
  source_tx_out.value,
  tx.hash as "txHash",
  tx_out_index as "index",
  source_tx.hash as "sourceTxHash",
  tx_in.tx_out_index as "sourceTxIndex"
from
  tx
join tx_in
  on tx_in.tx_in_id = tx.id
join tx_out as source_tx_out
  on tx_in.tx_out_id = source_tx_out.tx_id
  and tx_in.tx_out_index = source_tx_out.index
join tx as source_tx
  on source_tx_out.tx_id = source_tx.id;

create view "TransactionOutput" as
select
  address,
  value,
  tx.hash as "txHash",
  index
from tx
join tx_out
  on tx.id = tx_out.tx_id;

CREATE FUNCTION utxo_set_at_block("hash" hash32type)
RETURNS SETOF "TransactionOutput" AS $$
  select
    "TransactionOutput".address,
    "TransactionOutput".value,
    "TransactionOutput"."txHash",
    "TransactionOutput".index
  from tx
  join tx_out
    on tx.id = tx_out.tx_id
  join "TransactionOutput"
    on tx.hash = "TransactionOutput"."txHash"
  left outer join tx_in
    on tx_out.tx_id = tx_in.tx_out_id
    and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null
  and tx.block <= (select id from block where hash = "hash")
$$ LANGUAGE sql STABLE;
