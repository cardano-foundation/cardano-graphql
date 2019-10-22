-- Create all the views.

-- The views are always created because all views are deleted at the start ot the
-- migration process.

-- Conventions:
--  * VIEWs use TitleCase table names and camelCase columns for easy differentiation from tables.

-- The standard utxo view which shows all unspent transaction outputs.
create view "Utxo" as select
  address,
  value,
  tx.hash as "txId",
  index
from tx
join tx_out
  on tx.id = tx_out.tx_id
left outer join tx_in
  on tx_out.tx_id = tx_in.tx_out_id
  and tx_out.index = tx_in.tx_out_index
where tx_in.tx_in_id is null;

create view "TransactionOutput" as
select
  address,
  value,
  tx.hash as "txId",
  index
from tx
join tx_out
  on tx.id = tx_out.tx_id;

create view "TransactionInput" as
select
  source_tx_out.address,
  source_tx_out.value,
  tx.hash as "txId",
  source_tx.hash as "sourceTxId",
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


create view "Meta" as
select
  slot_duration as "slotDuration",
  start_time as "startTime",
  protocol_const as "protocolConst"
from meta
limit 1;

create view "Block" as
select
  CAST(COALESCE((select sum(tx.fee) from tx where tx.block = block.id), 0) as integer) as "fees",
  block."hash" as id,
  block.merkel_root as "merkelRootHash",
  block.block_no as number,
  previous_block."hash" as "previousBlockId",
  block.size as size,
  -- Even though we have epochNo defined in the Slot view,
  -- this is written by the node-client and makes identification
  -- of EBBs simpler, as EBBs don't have a slot_no
  block.epoch_no as "epochNo",
  block.slot_no as "slotNo",
  case when block.slot_no > 0	
    then block.slot_no - (block.epoch_no * (10 * (select protocol_const from meta)))	
    else 0	
  end as "slotWithinEpoch",
  block.time as "createdAt"
from block
left outer join block as previous_block
  on block.previous = previous_block.id;

create view "Transaction" as
select
  block.hash as "blockId",
  COALESCE(tx.fee, 0) as fee,
  tx.hash as id,
  cast((select sum("value") from tx_out where tx_id = tx.id) as bigint) as "totalOutput",
  block.time as "includedAt"
from
  tx
inner join block
  on block.id = tx.block;

create view "Epoch" as
select
  cast(sum(tx_out.value) as bigint) as output,
  count(distinct tx.hash) as "transactionsCount",
  block.epoch_no as "number",
  min(block.time) as "startedAt",
  max(block.time) as "lastBlockTime"
from block
join tx
  on tx.block = block.id
join tx_out
  on tx_out.tx_id = tx.id
where epoch_no is not null
group by block.epoch_no
order by block.epoch_no;

-- This function plays really nicely with Hasura,
-- and allows us to query the utxo set at any block height
-- https://docs.hasura.io/1.0/graphql/manual/queries/custom-functions.html
CREATE FUNCTION utxo_set_at_block("blockId" hash32type)
RETURNS SETOF "TransactionOutput" AS $$
  select
    "TransactionOutput".address,
    "TransactionOutput".value,
    "TransactionOutput"."txId",
    "TransactionOutput".index
  from tx
  join tx_out
    on tx.id = tx_out.tx_id
  join "TransactionOutput"
    on tx.hash = "TransactionOutput"."txId"
  left outer join tx_in
    on tx_out.tx_id = tx_in.tx_out_id
    and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null
  and tx.block <= (select id from block where hash = "blockId")
$$ LANGUAGE sql STABLE;

create view "Cardano" as
select
  number as "blockHeight",
  "epochNo" as "currentEpochNo"
from "Block"
where number is not null
order by number desc
limit 1;