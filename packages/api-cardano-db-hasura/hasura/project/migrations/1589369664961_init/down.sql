drop view if exists "Block", "Cardano", "Epoch", "Transaction", "Utxo", "TransactionInput", "TransactionOutput" cascade;
drop function if exists utxo_set_at_block cascade;
