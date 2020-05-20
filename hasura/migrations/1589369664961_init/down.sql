DROP VIEW IF EXISTS "Block", "Cardano", "Epoch", "Transaction", "Utxo", "TransactionInput", "TransactionOutput" CASCADE;
DROP FUNCTION IF EXISTS utxo_set_at_block CASCADE;