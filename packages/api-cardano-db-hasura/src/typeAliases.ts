import { Asset } from './graphql_types'
import { ConnectionConfig } from 'pg'

export type AssetMetadataHashAndId = Pick<Asset, 'metadataHash' | 'assetId'>

export type AssetMetadataAndHash = Pick<Asset, 'assetId' | 'decimals' | 'description' | 'logo' | 'name' | 'ticker' | 'url'> & { metadataHash: string }

export type AssetWithoutTokens = Omit<Asset, 'tokenMints' | 'tokenMints_aggregate'> & { firstAppearedInSlot: number }

export type DbConfig = Required<Pick<ConnectionConfig, 'database' | 'host' | 'password' | 'port' | 'user'>>;
