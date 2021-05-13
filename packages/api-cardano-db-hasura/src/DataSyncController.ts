import axios, { AxiosInstance } from 'axios'
import { DataFetcher } from '@cardano-graphql/util'
import AssetFingerprint from '@emurgo/cip14-js'
import { HostDoesNotExist } from './errors'
import hash from 'object-hash'
import { dummyLogger, Logger } from 'ts-log'
import { Db } from './Db'
import { HasuraClient } from './HasuraClient'
import { AssetWithoutTokens } from './typeAliases'

export interface Signature {
  signature: string
  publicKey: string
}

export const assetFingerprint = (asset: Pick<AssetWithoutTokens, 'assetName' | 'policyId'>) =>
  new AssetFingerprint(
    Buffer.from(asset.policyId, 'hex'),
    asset.assetName !== '' ? Buffer.from(asset.assetName, 'hex') : undefined)
    .fingerprint()

export interface AssetMetadata {
  description?: {
    value: string
    anSignatures: Signature[]
  }
  logo?: {
    value: string
    anSignatures: Signature[]
  }
  name?: {
    value: string
    anSignatures: Signature[]
  }
  owner?: Signature
  preImage?: {
    value: string
    hashFn: string
  }
  subject: string
  ticker?: {
    value: string
    anSignatures: Signature[]
  }
  url?: {
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
            const batchSize = 500
            const assetsWithoutMetadataCount = await this.hasuraClient.assetsWithoutMetadataCount({ _lte: 2 })
            this.logger.debug(
              { module: 'DataSyncController', qty: assetsWithoutMetadataCount },
              'Newly discovered assets missing metadata'
            )
            const batchQty = Math.ceil(assetsWithoutMetadataCount / batchSize)
            let totalCount = 0
            for (const i of Array(batchQty).keys()) {
              const assetsInBatch = await this.hasuraClient.getAssetsWithoutMetadata(
                { _lte: 3 }, { limit: batchSize, offset: batchSize * i }
              )
              if (assetsInBatch.length > 0) {
                await this.fetchAndApplyMetadata(assetsInBatch)
              }
              totalCount = totalCount + assetsInBatch.length
            }
            return totalCount
          },
          pollingInterval.initial * 10,
          this.logger
        ),
        ongoing: new DataFetcher<number>(
          'MetadataSynchronizerRefresh',
          async () => {
            const batchSize = 500
            const assetsEligibleForMetadataRefreshCount =
              await this.hasuraClient.assetsEligibleForMetadataRefreshCount({ _gt: 2 })
            this.logger.debug(
              { module: 'DataSyncController', qty: assetsEligibleForMetadataRefreshCount },
              'Assets eligible for metadata refresh'
            )
            const batchQty = Math.ceil(assetsEligibleForMetadataRefreshCount / batchSize)
            let totalCount = 0
            for (const i of Array(batchQty).keys()) {
              const assetsInBatch = await this.hasuraClient.getAssetsIncMetadata(
                { _gt: 2 }, { limit: batchSize, offset: batchSize * i }
              )
              if (assetsInBatch.length > 0) {
                await this.fetchAndApplyMetadata(assetsInBatch)
              }
              totalCount = totalCount + assetsInBatch.length
            }
            return totalCount
          },
          pollingInterval.ongoing * 10,
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
          { module: 'DataSyncController', qty: distinctAssetsInTokensCount },
          'Distinct assets in tokens count'
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
            {
              module: 'DataSyncController',
              batch: i,
              qty: assetsInBatch.length,
              existing: assetsAlreadyInDb.length
            },
            'Assets from tokens'
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
          this.logger.debug(
            { module: 'DataSyncController', qty: newAssets.length },
            'asset IDs diff'
          )
          if (newAssets.length > 0) {
            totalCount = totalCount + newAssets.length
            await this.hasuraClient.insertAssets(newAssets)
            this.logger.debug(
              { module: 'DataSyncController', batch: i, qty: newAssets.length },
              'synchronised assets table from tokens'
            )
          }
        }
        return totalCount
      },
      1200 * 1000,
      this.logger
    )
  }

  private async fetchAndApplyMetadata (assets: AssetWithoutTokens[]) {
    const newMetadata = await this.getAssetMetadata(assets)
    const assetsWithMetadata = newMetadata
      .map(metadata => ({
        metadata,
        metadataHash: hash(metadata),
        asset: assets.find(asset => asset.assetId === metadata.subject)
      }))
      .filter(({ asset, metadataHash }) => metadataHash !== asset.metadataHash)
      .map(obj => ({
        ...obj.asset,
        ...{
          description: obj.metadata.description?.value,
          logo: obj.metadata.logo?.value,
          name: obj.metadata.name?.value,
          ticker: obj.metadata.ticker?.value,
          url: obj.metadata.url?.value
        },
        ...{ metadataHash: hash(obj.metadata) }
      }))
    this.logger.debug(
      { module: 'DataSyncController', qty: assetsWithMetadata.length },
      'Metadata with updates to apply'
    )
    if (assetsWithMetadata.length > 0) {
      await this.hasuraClient.addMetadata(assetsWithMetadata)
    }
    await this.hasuraClient.incrementMetadataFetchAttempts(assets.map(asset => asset.assetId))
  }

  private async getAssetMetadata (assets: AssetWithoutTokens[]): Promise<AssetMetadata[]> {
    try {
      const response = await this.axiosClient.post('metadata/query', {
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
        this.logger.error({ err: error })
      } else {
        throw error
      }
    }
  }

  private async ensureAssetFingerprints (): Promise<void> {
    const runBatch = async (assets: Pick<AssetWithoutTokens, 'assetId' | 'assetName' | 'policyId'>[]) => {
      await this.hasuraClient.addAssetFingerprints(
        assets.map((asset) => ({ assetId: asset.assetId, fingerprint: assetFingerprint(asset) }))
      )
    }
    while (await this.hasuraClient.hasAssetsWithoutFingerprint()) {
      await runBatch(await this.hasuraClient.getAssetsWithoutFingerprint(2500))
    }
  }

  private async ensureMetadataServerIsAvailable (): Promise<void> {
    try {
      await this.axiosClient.get('/metadata/healthcheck')
    } catch (error) {
      if (error.code === 'ENOTFOUND') {
        throw new HostDoesNotExist('metadata server')
      } else if (error.response.status !== 404) {
        throw error
      }
    }
  }

  public async initialize () {
    this.logger.info({ module: 'DataSyncController' }, 'Initializing')
    await this.ensureAssetFingerprints()
    await this.assetSynchronizer.initialize()
    if (this.metadataServerUri) {
      await this.ensureMetadataServerIsAvailable()
      await this.metadataSynchronizer.initial.initialize()
      await this.metadataSynchronizer.ongoing.initialize()
    }
    this.logger.info({ module: 'DataSyncController' }, 'Initialized')
  }

  public async shutdown () {
    this.logger.info({ module: 'DataSyncController' }, 'Shutting down')
    if (this.metadataServerUri) {
      await this.metadataSynchronizer.initial.shutdown()
      await this.metadataSynchronizer.ongoing.shutdown()
    }
    await this.assetSynchronizer.shutdown()
    this.logger.info(
      { module: 'DataSyncController' },
      'Shutdown complete')
  }
}
