export type Maybe<T> = T | null;
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
};

export type Block = Entity & {
  __typename?: "Block";
  id: Scalars["ID"];
  transactions: Array<Maybe<Transaction>>;
};

export type Entity = {
  id: Scalars["ID"];
};

export type Ledger = {
  __typename?: "Ledger";
  blocks: Array<Maybe<Block>>;
  blockHeight: Scalars["Int"];
  transaction: Transaction;
  transactions: Array<Maybe<Transaction>>;
};

export type LedgerBlocksArgs = {
  id?: Maybe<Scalars["ID"]>;
};

export type LedgerTransactionArgs = {
  id: Scalars["ID"];
};

export type LedgerTransactionsArgs = {
  id?: Maybe<Scalars["ID"]>;
  ids?: Maybe<Array<Maybe<Scalars["ID"]>>>;
};

export type Mempool = {
  __typename?: "Mempool";
  transaction: Transaction;
  transactions: Array<Maybe<Transaction>>;
  transactionCount: Scalars["Int"];
};

export type MempoolTransactionArgs = {
  id: Scalars["ID"];
};

export type MempoolTransactionsArgs = {
  id: Scalars["ID"];
};

export type Outpoint = {
  __typename?: "Outpoint";
  txId: Scalars["String"];
  outputIndex: Scalars["Int"];
};

export type Query = {
  __typename?: "Query";
  transaction?: Maybe<Transaction>;
  transactions: Array<Maybe<Transaction>>;
};

export type QueryTransactionArgs = {
  id: Scalars["ID"];
};

export type QueryTransactionsArgs = {
  ids?: Maybe<Array<Maybe<Scalars["ID"]>>>;
};

export type Transaction = Entity & {
  __typename?: "Transaction";
  id: Scalars["ID"];
  fee: Scalars["Float"];
  inputs: Array<TransactionInput>;
  outputs: Array<TransactionOutput>;
};

export type TransactionInput = {
  __typename?: "TransactionInput";
  outpoint: Outpoint;
  address: Scalars["String"];
};

export type TransactionOutput = {
  __typename?: "TransactionOutput";
  value: Scalars["Int"];
  address: Scalars["String"];
};
