import { Asset } from './graphql_types'

export type AssetWithoutTokens = Omit<Asset, 'tokenMints' | 'tokenMints_aggregate'>
