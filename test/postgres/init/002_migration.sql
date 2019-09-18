-- TODO: Can be removed once new cexplorer received
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