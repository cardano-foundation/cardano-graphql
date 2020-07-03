drop view if exists "Transaction" cascade;
create view "Transaction" as
select
  block.hash as "blockHash",
  coalesce(tx.fee, 0) as fee,
  tx.hash as hash,
  cast((select sum("value") from tx_out where tx_id = tx.id) as bigint) as "totalOutput",
  tx.size,
  block.time as "includedAt"
from
  tx
inner join block
  on block.id = tx.block;
