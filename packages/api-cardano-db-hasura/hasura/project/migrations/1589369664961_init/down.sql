DROP VIEW if EXISTS
  "Block",
  "Cardano",
  "Delegation",
  "Epoch",
  "Reward",
  "SlotLeader",
  "StakeDeregistration",
  "StakePool",
  "StakeRegistration",
  "Transaction",
  "TransactionInput",
  "TransactionOutput",
  "Utxo",
  "Withdrawal" CASCADE;
DROP FUNCTION IF EXISTS utxo_set_at_block CASCADE;
