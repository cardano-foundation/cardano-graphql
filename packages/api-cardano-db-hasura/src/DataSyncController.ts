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
            const batchSize = 2500
            const assetsWithoutMetadataCount = await this.hasuraClient.assetsWithoutMetadataCount({ _lte: 2 })
            this.logger.debug(
              'new assets without metadata count',
              { module: 'DataSyncController', value: assetsWithoutMetadataCount }
            )
            const batchQty = Math.ceil(assetsWithoutMetadataCount / batchSize)
            let totalCount = 0
            for (const i of Array(batchQty).keys()) {
              const assetsInBatch = await this.hasuraClient.getAssetsWithoutMetadata(
                { _lte: 3 }, { limit: batchSize, offset: batchSize * i }
              )
              this.logger.debug(
                'assets without metadata in batch',
                {
                  module: 'DataSyncController',
                  value: { batch: i, qty: assetsInBatch.length }
                }
              )
              if (assetsInBatch.length > 0) {
                await this.fetchAndApplyMetadata(assetsInBatch, 'Assets missing metadata')
              }
              totalCount = totalCount + assetsInBatch.length
            }
            return totalCount
          },
          pollingInterval.initial,
          this.logger
        ),
        ongoing: new DataFetcher<number>(
          'MetadataSynchronizerRefresh',
          async () => {
            const batchSize = 2500
            const assetsEligibleForMetadataRefreshCount =
              await this.hasuraClient.assetsEligibleForMetadataRefreshCount({ _gt: 2 })
            this.logger.debug(
              'assets eligible for metadata refresh count',
              { module: 'DataSyncController', value: assetsEligibleForMetadataRefreshCount }
            )
            const batchQty = Math.ceil(assetsEligibleForMetadataRefreshCount / batchSize)
            let totalCount = 0
            for (const i of Array(batchQty).keys()) {
              const assetsInBatch = await this.hasuraClient.getAssetsIncMetadata(
                { _gt: 2 }, { limit: batchSize, offset: batchSize * i }
              )
              this.logger.debug(
                'assets without metadata in batch',
                {
                  module: 'DataSyncController',
                  value: { batch: i, qty: assetsInBatch.length }
                }
              )
              if (assetsInBatch.length > 0) {
                await this.fetchAndApplyMetadata(assetsInBatch, 'All assets')
              }
              totalCount = totalCount + assetsInBatch.length
            }
            return totalCount
          },
          pollingInterval.ongoing,
          this.logger
        )
      }
    }
    this.assetSynchronizer = new DataFetcher<number>(
      'AssetTableSynchronizer',
      async () => {
        const batchSize = 500
        const distinctAssetsInTokensCount = await this.hasuraClient.distinctAssetsInTokensCount()
        this.logger.debug(
          'distinct assets in tokens count',
          { module: 'DataSyncController', value: distinctAssetsInTokensCount }
        )
        const batchQty = Math.ceil(distinctAssetsInTokensCount / batchSize)
        let totalCount = 0
        for (const i of Array(batchQty).keys()) {
          const assetsInBatch = await this.hasuraClient.getDistinctAssetsInTokens(
            { limit: batchSize, offset: batchSize * i }
          )
          const assetsAlreadyInDb =
            await this.hasuraClient.getAssetsById(assetsInBatch.map(asset => asset.assetId))
          this.logger.debug(
            'asset IDs from tokens',
            {
              module: 'DataSyncController',
              value: { batch: i, qty: assetsInBatch.length, existing: assetsAlreadyInDb.length }
            }
          )
          const newAssets = assetsInBatch
            .filter(asset => assetsAlreadyInDb.find(existingAsset =>
              existingAsset.assetId === asset.assetId) === undefined)
            .map(asset => ({
              assetId: asset.assetId,
              assetName: asset.assetName,
              fingerprint: assetFingerprint(asset),
              policyId: asset.policyId,
              metadataFetchAttempts: 0
            }))
          this.logger.debug('asset IDs diff', { module: 'DataSyncController', value: newAssets.length })
          if (newAssets.length > 0) {
            totalCount = totalCount + newAssets.length
            await this.hasuraClient.insertAssets(newAssets)
            this.logger.debug(
              'synchronised assets table from tokens',
              { module: 'DataSyncController', value: { batch: i, qty: newAssets.length } }
            )
          }
        }
        return totalCount
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
      this.logger.debug(
        'Metadata with updates to apply',
        { module: 'DataSyncController', value: metadataWithAssetAndHash.length }
      )
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
    const runBatch = async (assets: Pick<Asset, 'assetId' | 'assetName' | 'policyId'>[]) => {
      for (const asset of assets) {
        const fingerprint = assetFingerprint(asset)
        await this.hasuraClient.addAssetFingerprint(asset.assetId, fingerprint)
      }
    }
    while (await this.hasuraClient.hasAssetsWithoutFingerprint()) {
      await runBatch(await this.hasuraClient.getAssetsWithoutFingerprint(2500))
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
