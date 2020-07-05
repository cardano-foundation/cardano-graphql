drop view "Block";
create view "Block" as
select
  cast(coalesce((select sum(tx.fee) from tx where tx.block = block.id), 0) as bigint) as "fees",
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
