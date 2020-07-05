drop view "Cardano";
create view "Cardano" as
select
  block_no as "tipBlockNo",
  epoch_no as "currentEpochNo",
  (select slot_duration from meta) as "slotDuration",
  (select slots_per_epoch from meta) as "slotsPerEpoch",
  (select start_time from meta) as "startTime",
  (select protocol_const from meta) as "protocolConst",
  (select network_name from meta) as "networkName"
from block
where block_no is not null
order by block_no desc
limit 1;