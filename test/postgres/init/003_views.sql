-- Create all the views.

-- The views are always created because all views are deleted at the start ot the
-- migration process.

-- Conventions:
--  * VIEWs use TitleCase table names and camelCase columns for easy differentiation from tables.

-- The standard utxo view which shows all unspent transaction outputs.
create view "Utxo" as select
  tx_out.*
  from tx_out left outer join tx_in
  on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null;

create view "TransactionOutput" as
select
  address,
  value,
  tx.hash as "txId",
  index
from
  tx
  inner join tx_out on tx.id = tx_out.tx_id;

create view "TransactionInput" as
select
  address,
  tx.hash as "sourceTxId",
  tx_in.tx_out_index as "sourceTxIndex",
  value
from
  tx
  inner join tx_out on tx.id = tx_out.tx_id
  inner join tx_in on tx_in.tx_out_id = tx.id;

create view "BlockMeta" as
-- The common table expression isn't strictly speaking needed,
-- but it does clean up this view quite a lot
with block_meta_cte as (
  select
    id,
    block_no,
    slot_no,
    epoch_no,
    quote_literal(block.slot_no * (select slot_duration from meta) * 0.001) as time_since_start,
    (select start_time from meta) as start_time,
    (select protocol_const from meta) as protocol_const
  from block
)
select
  id,
  block_no as "blockNo",
  slot_no as "slotNo",
  epoch_no as "epochNo",
  time_since_start as "secondsSinceGenesis",
  start_time as "timeSinceStart",
  case when slot_no >= 0
    then start_time + cast (time_since_start as interval)
    else start_time
  end as "createdAt",
  case when slot_no > 0
    then slot_no - (epoch_no * (10 * protocol_const))
    else 0
  end as "slotWithinEpoch"
from block_meta_cte;

create view "Block" as
select
  "BlockMeta"."createdAt",
  (select sum(fee) from tx where tx.block = block.id) as "fees",
  block."hash" as id,
  block.merkel_root as "merkelRootHash",
  block.block_no as number,
  previous_block."hash" as "previousBlock",
  block.size as size
from block
left outer join block as previous_block
  on block.previous = previous_block.id
inner join "BlockMeta"
  on "BlockMeta".id = block.id;

create view "Transaction" as
select
  block.hash as "block",
  tx.fee,
  tx.hash as id,
  "BlockMeta"."createdAt" as "includedAt",
  (select sum("value") from tx_out where tx_id = tx.id) as "totalOutput"
from
  tx
inner join "BlockMeta"
  on "BlockMeta".id = tx.block
inner join block
  on block.id = tx.block;

create view "Epoch" as 
select
  sum(tx_out.value) as output,
  max("Block"."createdAt") as "endedAt",
  min("Block"."createdAt") as "startedAt",
  count(distinct tx.hash) as "transactionsCount",
  "BlockMeta"."epochNo" as "number"
from block
join "Block"
  on block.hash = "Block".id
join tx
  on tx.block = block.id
join tx_out
  on tx_out.tx_id = tx.id
join "BlockMeta"
  on "BlockMeta".id = block.id
group by "BlockMeta"."epochNo"
order by "BlockMeta"."epochNo";

create view "Slot" as 
with recursive slot_numbers as
(
  select 0 as slot_no
  union all
  select slot_no + 1 FROM slot_numbers
  where slot_no + 1 <= (select max(slot_no) from block)
)
select
  slot_numbers.slot_no as "number",
  block.hash as block,
  block.slot_leader as leader,
  "BlockMeta"."epochNo",
  "BlockMeta"."createdAt" -- This is currently null if there is no block. Lift date calc here
from slot_numbers
left outer join block
  on block.slot_no = slot_numbers.slot_no
left outer join "BlockMeta"
  on block.id = "BlockMeta".id;

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