ALTER TABLE "block" ADD COLUMN "epoch_no" uinteger NULL;

-- Hand written SQL to insert the epoch_no field.
-- Update all non-EBBs.
UPDATE block SET epoch_no = div (slot_no, 21600) WHERE slot_no IS NOT NULL ;

-- Update EBBs.
UPDATE block SET epoch_no =
  CASE WHEN previous_block.epoch_no IS NULL
    THEN 0
    ELSE previous_block.epoch_no + 1
    END
  FROM block AS previous_block
  WHERE block.previous = previous_block.id AND block.epoch_no IS NULL ;

ALTER TABLE "block" ADD COLUMN "time" timestamp NULL;

-- Hand written SQL to populate the new column and then make it non-NULL.
-- First timestamp the genesis block.
update block set time = (select start_time from meta) where epoch_no is null;

-- Then add timestamps to the EBBs.
update block
  set time =
    (select cast (quote_literal (epoch_no * (select slot_duration from meta) * 21.6) as interval)
        + (select start_time from meta))
  where slot_no is null and epoch_no is not null;

-- Then add timestamp to the main blocks.
update block
  set time =
    (select cast (quote_literal (slot_no * (select slot_duration from meta) * 0.001) as interval)
        + (select start_time from meta))
  where slot_no is not null;

-- Finally make the timestamp non-NULLable.
alter table block alter column time set not null;