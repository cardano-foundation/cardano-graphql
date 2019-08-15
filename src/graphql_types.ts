import { GraphQLResolveInfo } from "graphql";
import { Context } from "./Context";
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
  transaction: Transaction;
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

export type ResolverTypeWrapper<T> = Promise<T> | T;

export type ResolverFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => Promise<TResult> | TResult;

export type StitchingResolver<TResult, TParent, TContext, TArgs> = {
  fragment: string;
  resolve: ResolverFn<TResult, TParent, TContext, TArgs>;
};

export type Resolver<TResult, TParent = {}, TContext = {}, TArgs = {}> =
  | ResolverFn<TResult, TParent, TContext, TArgs>
  | StitchingResolver<TResult, TParent, TContext, TArgs>;

export type SubscriptionSubscribeFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => AsyncIterator<TResult> | Promise<AsyncIterator<TResult>>;

export type SubscriptionResolveFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;

export interface SubscriptionResolverObject<TResult, TParent, TContext, TArgs> {
  subscribe: SubscriptionSubscribeFn<TResult, TParent, TContext, TArgs>;
  resolve?: SubscriptionResolveFn<TResult, TParent, TContext, TArgs>;
}

export type SubscriptionResolver<
  TResult,
  TParent = {},
  TContext = {},
  TArgs = {}
> =
  | ((
      ...args: any[]
    ) => SubscriptionResolverObject<TResult, TParent, TContext, TArgs>)
  | SubscriptionResolverObject<TResult, TParent, TContext, TArgs>;

export type TypeResolveFn<TTypes, TParent = {}, TContext = {}> = (
  parent: TParent,
  context: TContext,
  info: GraphQLResolveInfo
) => Maybe<TTypes>;

export type NextResolverFn<T> = () => Promise<T>;

export type DirectiveResolverFn<
  TResult = {},
  TParent = {},
  TContext = {},
  TArgs = {}
> = (
  next: NextResolverFn<TResult>,
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;

/** Mapping between all available schema types and the resolvers types */
export type ResolversTypes = {
  Query: ResolverTypeWrapper<{}>;
  ID: ResolverTypeWrapper<Scalars["ID"]>;
  Transaction: ResolverTypeWrapper<Transaction>;
  Entity: ResolverTypeWrapper<Entity>;
  Float: ResolverTypeWrapper<Scalars["Float"]>;
  TransactionInput: ResolverTypeWrapper<TransactionInput>;
  Outpoint: ResolverTypeWrapper<Outpoint>;
  String: ResolverTypeWrapper<Scalars["String"]>;
  Int: ResolverTypeWrapper<Scalars["Int"]>;
  TransactionOutput: ResolverTypeWrapper<TransactionOutput>;
  Boolean: ResolverTypeWrapper<Scalars["Boolean"]>;
  Ledger: ResolverTypeWrapper<Ledger>;
  Block: ResolverTypeWrapper<Block>;
  Mempool: ResolverTypeWrapper<Mempool>;
};

/** Mapping between all available schema types and the resolvers parents */
export type ResolversParentTypes = {
  Query: {};
  ID: Scalars["ID"];
  Transaction: Transaction;
  Entity: Entity;
  Float: Scalars["Float"];
  TransactionInput: TransactionInput;
  Outpoint: Outpoint;
  String: Scalars["String"];
  Int: Scalars["Int"];
  TransactionOutput: TransactionOutput;
  Boolean: Scalars["Boolean"];
  Ledger: Ledger;
  Block: Block;
  Mempool: Mempool;
};

export type BlockResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["Block"]
> = {
  id?: Resolver<ResolversTypes["ID"], ParentType, ContextType>;
  transactions?: Resolver<
    Array<Maybe<ResolversTypes["Transaction"]>>,
    ParentType,
    ContextType
  >;
};

export type EntityResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["Entity"]
> = {
  __resolveType: TypeResolveFn<
    "Transaction" | "Block",
    ParentType,
    ContextType
  >;
  id?: Resolver<ResolversTypes["ID"], ParentType, ContextType>;
};

export type LedgerResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["Ledger"]
> = {
  blocks?: Resolver<
    Array<Maybe<ResolversTypes["Block"]>>,
    ParentType,
    ContextType,
    LedgerBlocksArgs
  >;
  blockHeight?: Resolver<ResolversTypes["Int"], ParentType, ContextType>;
  transaction?: Resolver<
    ResolversTypes["Transaction"],
    ParentType,
    ContextType,
    LedgerTransactionArgs
  >;
  transactions?: Resolver<
    Array<Maybe<ResolversTypes["Transaction"]>>,
    ParentType,
    ContextType,
    LedgerTransactionsArgs
  >;
};

export type MempoolResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["Mempool"]
> = {
  transaction?: Resolver<
    ResolversTypes["Transaction"],
    ParentType,
    ContextType,
    MempoolTransactionArgs
  >;
  transactions?: Resolver<
    Array<Maybe<ResolversTypes["Transaction"]>>,
    ParentType,
    ContextType,
    MempoolTransactionsArgs
  >;
  transactionCount?: Resolver<ResolversTypes["Int"], ParentType, ContextType>;
};

export type OutpointResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["Outpoint"]
> = {
  txId?: Resolver<ResolversTypes["String"], ParentType, ContextType>;
  outputIndex?: Resolver<ResolversTypes["Int"], ParentType, ContextType>;
};

export type QueryResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["Query"]
> = {
  transaction?: Resolver<
    ResolversTypes["Transaction"],
    ParentType,
    ContextType,
    QueryTransactionArgs
  >;
  transactions?: Resolver<
    Array<Maybe<ResolversTypes["Transaction"]>>,
    ParentType,
    ContextType,
    QueryTransactionsArgs
  >;
};

export type TransactionResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["Transaction"]
> = {
  id?: Resolver<ResolversTypes["ID"], ParentType, ContextType>;
  fee?: Resolver<ResolversTypes["Float"], ParentType, ContextType>;
  inputs?: Resolver<
    Array<ResolversTypes["TransactionInput"]>,
    ParentType,
    ContextType
  >;
  outputs?: Resolver<
    Array<ResolversTypes["TransactionOutput"]>,
    ParentType,
    ContextType
  >;
};

export type TransactionInputResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["TransactionInput"]
> = {
  outpoint?: Resolver<ResolversTypes["Outpoint"], ParentType, ContextType>;
  address?: Resolver<ResolversTypes["String"], ParentType, ContextType>;
};

export type TransactionOutputResolvers<
  ContextType = Context,
  ParentType = ResolversParentTypes["TransactionOutput"]
> = {
  value?: Resolver<ResolversTypes["Int"], ParentType, ContextType>;
  address?: Resolver<ResolversTypes["String"], ParentType, ContextType>;
};

export type Resolvers<ContextType = Context> = {
  Block?: BlockResolvers<ContextType>;
  Entity?: EntityResolvers;
  Ledger?: LedgerResolvers<ContextType>;
  Mempool?: MempoolResolvers<ContextType>;
  Outpoint?: OutpointResolvers<ContextType>;
  Query?: QueryResolvers<ContextType>;
  Transaction?: TransactionResolvers<ContextType>;
  TransactionInput?: TransactionInputResolvers<ContextType>;
  TransactionOutput?: TransactionOutputResolvers<ContextType>;
};

/**
 * @deprecated
 * Use "Resolvers" root object instead. If you wish to get "IResolvers", add "typesPrefix: I" to your config.
 */
export type IResolvers<ContextType = Context> = Resolvers<ContextType>;
