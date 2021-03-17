import axios, { AxiosInstance } from 'axios'
import { chunkArray, DataFetcher } from '@cardano-graphql/util'
import AssetFingerprint from '@emurgo/cip14-js'
import { Asset } from './graphql_types'
import { HostDoesNotExist } from './errors'
import hash from 'object-hash'
import { dummyLogger, Logger } from 'ts-log'
import { Db } from './Db'
import { HasuraClient } from './HasuraClient'

export interface Signature {
  signature: string
  publicKey: string
}

export const assetFingerprint = (asset: Pick<Asset, 'assetName' | 'policyId'>) =>
  new AssetFingerprint(
    Buffer.from(asset.policyId, 'hex'),
    asset.assetName !== '' ? Buffer.from(asset.assetName, 'hex') : undefined)
    .fingerprint()

export interface AssetMetadata {
  description: {
    value: string
    anSignatures: Signature[]
  }
  logo: {
    value: string
    anSignatures: Signature[]
  }
  name: {
    value: string
    anSignatures: Signature[]
  }
  owner?: Signature
  preImage?: {
    value: string
    hashFn: string
  }
  subject: string
  ticker: {
    value: string
    anSignatures: Signature[]
  }
  url: {
    value: string
    anSignatures: Signature[]
  }
}

export class DataSyncController {
  private axiosClient: AxiosInstance
  private assetSynchronizer: DataFetcher<number>
  private metadataSynchronizer: {
    initial: DataFetcher<number>
    ongoing: DataFetcher<number>
  }

  constructor (
    readonly hasuraClient: HasuraClient,
    readonly db: Db,
    pollingInterval: {
      initial: number
      ongoing: number
    },
    private logger: Logger = dummyLogger,
    private metadataServerUri?: string
  ) {
    if (this.metadataServerUri) {
      this.axiosClient = axios.create({
        baseURL: metadataServerUri
      })
      this.metadataSynchronizer = {
        initial: new DataFetcher<number>(
          'MetadataSynchronizer',
          async () => {
            const assets = await this.hasuraClient.getAssetsWithoutMetadata({ _lte: 5 })
            if (assets.length > 0) {
              await this.fetchAndApplyMetadata(assets, 'Assets missing metadata')
            }
            return assets.length
          },
          pollingInterval.initial,
          this.logger
        ),
        ongoing: new DataFetcher<number>(
          'MetadataSynchronizerRefresh',
          async () => {
            const assets = await this.hasuraClient.getAssetsIncMetadata({ _gt: 5 })
            if (assets.length > 0) {
              await this.fetchAndApplyMetadata(assets, 'All assets')
            }
            return assets.length
          },
          pollingInterval.ongoing,
          this.logger
        )
      }
    }
    this.assetSynchronizer = new DataFetcher<number>(
      'AssetTableSynchronizer',
      async () => {
        const distinctAssetsInTokens = await this.hasuraClient.getDistinctAssetsInTokens()
        this.logger.debug('distinct asset IDs from tokens', { module: 'DataSyncController', value: distinctAssetsInTokens.length })
        const assetIds = await this.hasuraClient.getAssetIds()
        this.logger.debug('fetched asset IDs', { module: 'DataSyncController', value: assetIds.length })
        const diff = distinctAssetsInTokens
          .filter(asset => !assetIds.includes(asset.assetId))
          .map(asset => ({
            assetId: asset.assetId,
            assetName: asset.assetName,
            fingerprint: assetFingerprint(asset),
            policyId: asset.policyId,
            metadataFetchAttempts: 0
          })
          )
        this.logger.debug('asset IDs diff', { module: 'DataSyncController', value: diff.length })
        if (diff.length > 0) {
          await this.hasuraClient.insertAssets(diff)
          this.logger.debug('synchronised assets table from tokens', { module: 'DataSyncController', value: diff.length })
        }
        return diff.length
      },
      60 * 1000,
      this.logger
    )
  }

  private async fetchAndApplyMetadata (assets: Asset[], assetsDescriptor: string) {
    this.logger.debug(assetsDescriptor, { module: 'DataSyncController', value: assets.length })
    const assetBatches = chunkArray<Asset>(assets, 250)
    for (const batch of assetBatches) {
      const newMetadata = await this.getAssetMetadata(batch)
      const metadataWithAssetAndHash = newMetadata
        .map(metadata => ({
          metadata,
          metadataHash: hash(metadata),
          asset: assets.find(asset => asset.assetId === metadata.subject)
        }))
        .filter(({ asset, metadataHash }) => metadataHash !== asset.metadataHash)
      this.logger.debug('Metadata with updates to apply', { module: 'DataSyncController', value: metadataWithAssetAndHash.length })
      if (metadataWithAssetAndHash.length > 0) {
        for (const { metadata, metadataHash } of metadataWithAssetAndHash) {
          await this.hasuraClient.addMetadata(metadata, metadataHash)
        }
      }
      for (const asset of batch) {
        await this.hasuraClient.incrementMetadataFetchAttempts(asset.assetId)
      }
    }
  }

  private async getAssetMetadata (assets: Asset[]): Promise<AssetMetadata[]> {
    try {
      const response = await this.axiosClient.post('query', {
        subjects: assets.map(asset => asset.assetId),
        properties: [
          'description',
          'logo',
          'name',
          'ticker',
          'url'
        ]
      })
      return response.data.subjects
    } catch (error) {
      if (error.code === 'ENOTFOUND') {
        this.logger.error(error.message)
      } else {
        throw error
      }
    }
  }

  private async ensureAssetFingerprints (): Promise<void> {
    const assets = await this.hasuraClient.getAssetsWithoutFingerprint()
    this.logger.debug('Assets without fingerprint', { module: 'DataSyncController', value: assets.length })
    for (const asset of assets) {
      const fingerprint = assetFingerprint(asset)
      this.logger.debug('Asset', { module: 'DataSyncController', value: { assetId: asset.assetId, fingerprint } })
      await this.hasuraClient.addAssetFingerprint(asset.assetId, fingerprint)
    }
  }

  private async ensureMetadataServerIsAvailable (): Promise<void> {
    try {
      await this.axiosClient.get('healthcheck')
    } catch (error) {
      if (error.code === 'ENOTFOUND') {
        throw new HostDoesNotExist('metadata server')
      } else if (error.response.status !== 404) {
        throw error
      }
    }
  }

  public async initialize () {
    this.logger.info('Initializing', { module: 'DataSyncController' })
    await this.ensureAssetFingerprints()
    await this.assetSynchronizer.initialize()
    if (this.metadataServerUri) {
      await this.ensureMetadataServerIsAvailable()
      await this.metadataSynchronizer.initial.initialize()
      await this.metadataSynchronizer.ongoing.initialize()
    }
    this.logger.info('Initialized', { module: 'DataSyncController' })
  }

  public async shutdown () {
    this.logger.info('Shutting down', { module: 'DataSyncController' })
    if (this.metadataServerUri) {
      await this.metadataSynchronizer.initial.shutdown()
      await this.metadataSynchronizer.ongoing.shutdown()
    }
    await this.assetSynchronizer.shutdown()
    this.logger.info('Shutdown complete', { module: 'DataSyncController' })
  }
}
