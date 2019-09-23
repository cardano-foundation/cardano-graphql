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
  tx.hash as "sourceTxId",
  tx_in.tx_out_index as "sourceTxIndex"
from
  tx
join tx_in
  on tx_in.tx_in_id = tx.id
join tx_out as source_tx_out
  on tx_in.tx_out_id = source_tx_out.tx_id
  and tx_in.tx_out_index = source_tx_out.index;

create view "Slot" as 
with recursive slot_numbers as
(
  select 0 as slot
  union all
  select slot + 1 from slot_numbers
  where slot + 1 <= (select max(slot_no) from block)
), slot_meta as (
  select 
    slot,
    quote_literal(slot * (select slot_duration from meta) * 0.001) as time_since_start,
    (select start_time from meta) as start_time,
    (select protocol_const from meta) as protocol_const
  from slot_numbers
)
select
  slot as "number",
  block.hash as block,
  block.slot_leader as leader,
  case when slot > 0
    then floor(slot / (10 * protocol_const))
    else 0	
  end as "epochNo",
  case when slot >= 0
    then start_time + cast (time_since_start as interval)
    else start_time
  end as "createdAt",
  case when slot > 0
    then slot - (epoch_no * (10 * protocol_const))
    else 0
  end as "slotWithinEpoch"
from slot_meta
left outer join block
  on block.slot_no = slot_meta.slot;

create view "Block" as
select
  (select sum(fee) from tx where tx.block = block.id) as "fees",
  block."hash" as id,
  block.merkel_root as "merkelRootHash",
  block.block_no as number,
  previous_block."hash" as "previousBlock",
  block.size as size,
  -- Even though we have epochNo defined in the Slot view,
  -- this is written by the node-client and makes identification
  -- of EBBs simpler, as EBBs don't have a slot_no
  block.epoch_no as "epochNo"
from block
left outer join block as previous_block
  on block.previous = previous_block.id;

create view "Transaction" as
select
  block.hash as "block",
  tx.fee,
  tx.hash as id,
  "Slot"."createdAt" as "includedAt",
  (select sum("value") from tx_out where tx_id = tx.id) as "totalOutput"
from
  tx
inner join block
  on block.id = tx.block
inner join "Slot"
  on "Slot".number = block.slot_no;

create view "Epoch" as 
select
  sum(tx_out.value) as output,
  max("Slot"."createdAt") as "endedAt",
  min("Slot"."createdAt") as "startedAt",
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