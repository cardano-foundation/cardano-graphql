import { Asset } from './graphql_types'

export type AssetMetadataHashAndId = Pick<Asset, 'metadataHash' | 'assetId'>

export type AssetMetadataAndHash = Pick<Asset, 'assetId' | 'description' | 'logo' | 'name' | 'ticker' | 'url'> & { metadataHash: string }

export type AssetWithoutTokens = Omit<Asset, 'tokenMints' | 'tokenMints_aggregate'> & { firstAppearedInSlot: number }
