drop view "Block";
CREATE VIEW "public"."Block" AS
 SELECT (COALESCE(( SELECT sum((tx.fee)::bigint) AS sum
           FROM tx
          WHERE (tx.block = block.id)), (0)::numeric))::bigint AS fees,
    block.hash,
    block.merkel_root AS "merkelRoot",
    block.block_no AS number,
    previous_block.hash AS "previousBlockHash",
    next_block.hash AS "nextBlockHash",
    slot_leader.description AS "createdBy",
    block.size,
    block.tx_count AS "transactionsCount",
    block.epoch_no AS "epochNo",
    block.slot_no AS "slotNo",
    block.slot_no % (SELECT slots_per_epoch FROM meta) AS "slotInEpoch",
    block."time" AS "createdAt"
   FROM (((block
     LEFT JOIN block previous_block ON ((block.previous = previous_block.id)))
     LEFT JOIN block next_block ON ((next_block.previous = block.id)))
     LEFT JOIN slot_leader ON ((block.slot_leader = slot_leader.id)));
