export type Maybe<T> = T | null;
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
};

export type Outpoint = {
  __typename?: "Outpoint";
  txId: Scalars["String"];
  outputIndex: Scalars["Int"];
};

export type Query = {
  __typename?: "Query";
  transaction: Transaction;
  transactions: Array<Transaction>;
};

export type QueryTransactionArgs = {
  id: Scalars["ID"];
};

export type QueryTransactionsArgs = {
  ids: Array<Scalars["ID"]>;
};

export type Transaction = {
  __typename?: "Transaction";
  id: Scalars["String"];
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
