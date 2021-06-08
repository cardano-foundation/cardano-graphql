import axios, { AxiosInstance } from 'axios'
import { errors } from '@cardano-graphql/util'
import hash from 'object-hash'
import { dummyLogger, Logger } from 'ts-log'
import { AssetMetadata } from './AssetMetadata'
import { Config } from './Config'
import { Asset } from './graphql_types'
import { HasuraClient } from './HasuraClient'
import PgBoss, { JobWithDoneCallback } from 'pg-boss'

const ASSET_METADATA_FETCH_INITIAL = 'asset-metadata-fetch-initial'
const ASSET_METADATA_FETCH_UPDATE = 'asset-metadata-fetch-update'
const SIX_HOURS = 21600

type AssetJobPayload = { assetId: string }
const MODULE_NAME = 'Worker'

export class Worker {
  private axiosClient: AxiosInstance
  private queue: PgBoss
  private isInitialized: boolean

  constructor (
    readonly hasuraClient: HasuraClient,
    private logger: Logger = dummyLogger,
    private metadataServerUri: string,
    queueConfig: Config['db'],
    private options?: {
      metadataUpdateInterval?: {
        assets?: Config['metadataUpdateInterval']['assets']
      }
    }
  ) {
    this.isInitialized = false
    this.queue = new PgBoss({
      application_name: 'cardano-graphql',
      ...queueConfig
    })
    this.axiosClient = axios.create({
      baseURL: this.metadataServerUri
    })
  }

  private async getAssetMetadata (assetIds: Asset['assetId'][]): Promise<AssetMetadata[]> {
    try {
      const response = await this.axiosClient.post('metadata/query', {
        subjects: assetIds,
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

  private async ensureMetadataServerIsAvailable (): Promise<void> {
    try {
      await this.axiosClient.get('/metadata/healthcheck')
    } catch (error) {
      if (error.code === 'ENOTFOUND') {
        throw new errors.HostDoesNotExist('metadata server')
      } else if (error.response.status !== 404) {
        throw error
      }
    }
  }

  public async initialize () {
    this.logger.info({ module: MODULE_NAME }, 'Initializing')
    await this.ensureMetadataServerIsAvailable()
    this.isInitialized = true
    this.logger.info({ module: MODULE_NAME }, 'Initialized')
  }

  public async start () {
    if (!this.isInitialized) {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'start')
    }
    this.logger.info({ module: MODULE_NAME }, 'Starting')
    const subscriptionHandler: PgBoss.SubscribeHandler<AssetJobPayload, void> = async (data: JobWithDoneCallback<AssetJobPayload, void>) => {
      // The TypeDef doesn't cover the valid batch data, so a user-defined guard is used.
      if ('length' in data) {
        const jobs = data as JobWithDoneCallback<AssetJobPayload, AssetJobPayload>[]
        this.logger.debug({ module: MODULE_NAME, qty: jobs.length }, 'Processing jobs')
        const assetIds = jobs.map((job) => job.data.assetId)
        const fetchedMetadata = await this.getAssetMetadata(assetIds)
        const existingAssetMetadataHashes = await this.hasuraClient.getAssetMetadataHashesById(assetIds)
        for (const job of jobs) {
          const assetId = job.data.assetId
          const metadata = fetchedMetadata.find(item => item.subject === assetId)
          if (metadata === undefined) {
            this.logger.trace(
              { module: MODULE_NAME, assetId },
              'Metadata not found in registry. Will retry'
            )
            job.done(new Error(`Metadata for asset ${assetId} not found in registry`))
          } else {
            const existingAssetMetadataHashObj = existingAssetMetadataHashes.find(
              item => item.assetId === assetId)
            const metadataHash = hash(metadata)
            if (existingAssetMetadataHashObj?.metadataHash === metadataHash) {
              this.logger.trace(
                { module: MODULE_NAME, assetId },
                'Metadata from registry matches local'
              )
            } else {
              this.logger.trace({ module: MODULE_NAME, assetId }, 'Found metadata in registry')
              await this.hasuraClient.addAssetMetadata({
                assetId,
                description: metadata.description?.value,
                logo: metadata.logo?.value,
                name: metadata.name?.value,
                ticker: metadata.ticker?.value,
                url: metadata.url?.value,
                metadataHash
              })
              await this.queue.publishAfter(ASSET_METADATA_FETCH_UPDATE, { assetId }, {
                retryDelay: this.options?.metadataUpdateInterval?.assets ?? SIX_HOURS
              }, this.options?.metadataUpdateInterval?.assets ?? SIX_HOURS)
            }
          }
        }
      }
    }
    await this.queue.start()
    await this.queue.subscribe<AssetJobPayload, void>(ASSET_METADATA_FETCH_INITIAL,
      {
        batchSize: 500,
        newJobCheckIntervalSeconds: 5
      },
      subscriptionHandler)
    await this.queue.subscribe<AssetJobPayload, void>(ASSET_METADATA_FETCH_UPDATE,
      {
        batchSize: 500,
        newJobCheckIntervalSeconds: 5
      },
      subscriptionHandler)
    this.logger.info({ module: MODULE_NAME }, 'Started')
  }

  public async shutdown () {
    if (!this.isInitialized) {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'shutdown')
    }
    this.logger.info({ module: MODULE_NAME }, 'Shutting down')
    await Promise.all([
      this.queue.unsubscribe(ASSET_METADATA_FETCH_INITIAL),
      this.queue.unsubscribe(ASSET_METADATA_FETCH_UPDATE)
    ])
    this.logger.info({ module: MODULE_NAME }, 'Shutdown complete')
  }
}
