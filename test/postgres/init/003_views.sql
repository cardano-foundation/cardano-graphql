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


create view "Slot" as
with slot_meta as (
  select
    slot_no,
    quote_literal(slot_no * (select slot_duration from meta) * 0.001) as time_since_start,
    (select start_time from meta) as start_time,
    (select protocol_const from meta) as protocol_const
  from block
)
select
  slot_meta.slot_no as "number",
  block.hash as "blockId",
  slot_leader.hash as leader,
  case when slot_meta.slot_no > 0
    then CAST(floor(slot_meta.slot_no / (10 * protocol_const)) AS INT)
    else 0
  end as "epochNo",
  case when slot_meta.slot_no >= 0
    then start_time + cast (time_since_start as interval)
    else start_time
  end as "startedAt",
  case when slot_meta.slot_no > 0
    then slot_meta.slot_no - (epoch_no * (10 * protocol_const))
    else 0
  end as "slotWithinEpoch"
from slot_meta
join block
  on slot_meta.slot_no = block.slot_no
left outer join slot_leader
  on block.slot_leader = slot_leader.id;

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
  block.slot_no as "slotNo"
from block
left outer join block as previous_block
  on block.previous = previous_block.id;

create view "Transaction" as
select
  block.hash as "blockId",
  COALESCE(tx.fee, 0) as fee,
  tx.hash as id,
  "Slot"."startedAt" as "includedAt",
  cast((select sum("value") from tx_out where tx_id = tx.id) as bigint) as "totalOutput"
from
  tx
inner join block
  on block.id = tx.block
-- Left join here to include transactions in the genesis block
left outer join "Slot"
  on "Slot".number = block.slot_no;

create view "Epoch" as
select
  cast(sum(tx_out.value) as bigint) as output,
  max("Slot"."startedAt") as "endedAt",
  min("Slot"."startedAt") as "startedAt",
  count(distinct tx.hash) as "transactionsCount",
  "Slot"."epochNo" as "number"
from "Slot"
left outer join block
  on block.slot_no = "Slot".number
join tx
  on tx.block = block.id
join tx_out
  on tx_out.tx_id = tx.id
group by "Slot"."epochNo"
order by "Slot"."epochNo";

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