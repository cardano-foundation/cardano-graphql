create or replace view "Transaction" as
select
  block.hash as "blockHash",
  COALESCE(tx.fee, 0) as fee,
  tx.hash as hash,
  cast((select sum("value") from tx_out where tx_id = tx.id) as bigint) as "totalOutput",
  tx.size,
  block.time as "includedAt",
  tx.block_index AS "blockIndex"
from
  tx
inner join block
  on block.id = tx.block;
