import { errors, RunnableModuleState } from '@cardano-graphql/util'
import hash from 'object-hash'
import { dummyLogger, Logger } from 'ts-log'
import PgBoss, { JobWithDoneCallback } from 'pg-boss'
import { AssetV2 } from './AssetMetadata'
import { MetadataClient } from './MetadataClient'
import { DbConfig } from './typeAliases'
import { HasuraBackgroundClient } from './HasuraBackgroundClient'

const ASSET_METADATA_FETCH_INITIAL = 'asset-metadata-fetch-initial'
const ASSET_METADATA_FETCH_UPDATE = 'asset-metadata-fetch-update'
const SIX_HOURS = 21600

type AssetJobPayload = { assetId: string }
const MODULE_NAME = 'Worker'

// Convert hex-encoded logo (CIP-26) to base64 for backward compatibility
// CIP-68 logos are URLs and should be returned as-is
const processLogoValue = (logoValue: string): string => {
  // If it's a URL (CIP-68), return as-is
  if (
    logoValue.startsWith('http://') ||
    logoValue.startsWith('https://') ||
    logoValue.startsWith('ipfs://')
  ) {
    return logoValue
  }
  // Otherwise, it's hex-encoded (CIP-26), convert to base64
  return Buffer.from(logoValue, 'hex').toString('base64')
}

export class Worker {
  private queue: PgBoss
  private state: RunnableModuleState

  constructor (
    readonly hasuraClient: HasuraBackgroundClient,
    private logger: Logger = dummyLogger,
    private metadataFetchClient: MetadataClient,
    private queueConfig: DbConfig,
    private options?: {
      metadataUpdateInterval?: {
        assets?: number
      }
    }
  ) {
    this.state = 'initialized'
  }

  public async initQueue (): Promise<void> {
    if (this.queue) return
    this.queue = new PgBoss({
      application_name: 'cardano-graphql',
      ...this.queueConfig
    })
    await this.queue.start()
  }

  public async start () {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'start')
    }
    this.logger.info({ module: MODULE_NAME }, 'Starting')
    await this.initQueue()
    const subscriptionHandler: PgBoss.SubscribeHandler<
      AssetJobPayload,
      void
    > = async (data: object) => {
      // The TypeDef doesn't cover the valid batch data, so a user-defined guard is used.
      if ('length' in data) {
        const jobs = data as JobWithDoneCallback<AssetJobPayload, void>[]
        this.logger.debug(
          { module: MODULE_NAME, qty: jobs.length },
          'Processing jobs'
        )
        const assetIds = jobs.map((job) => job.data.assetId)
        let fetchedMetadata: AssetV2[]
        let existingAssetMetadataHashes: Awaited<ReturnType<typeof this.hasuraClient.getAssetMetadataHashesById>>
        try {
          fetchedMetadata = await this.metadataFetchClient.fetch(assetIds)
          existingAssetMetadataHashes =
            await this.hasuraClient.getAssetMetadataHashesById(assetIds)
        } catch (error) {
          this.logger.warn(
            { module: MODULE_NAME },
            `Batch fetch failed, rescheduling all jobs: ${error.message}`
          )
          const updateInterval = this.options?.metadataUpdateInterval?.assets ?? SIX_HOURS
          for (const job of jobs) {
            job.done(error)
            await this.queue.publishAfter(
              ASSET_METADATA_FETCH_UPDATE,
              { assetId: job.data.assetId },
              { retryDelay: updateInterval },
              updateInterval
            )
          }
          return
        }
        for (const job of jobs) {
          const assetId = job.data.assetId
          const subject = fetchedMetadata.find(
            (item: any) => item.subject === assetId
          )
          if (subject === undefined) {
            this.logger.trace(
              { module: MODULE_NAME, assetId },
              'Metadata not found in registry. Will retry'
            )
            job.done(
              new Error(`Metadata for asset ${assetId} not found in registry`)
            )
            // INITIAL jobs are retried by pgboss via retryLimit/retryDelay.
            // UPDATE jobs have no retryLimit, so reschedule manually to keep the cycle alive.
            if (job.name === ASSET_METADATA_FETCH_UPDATE) {
              const updateInterval = this.options?.metadataUpdateInterval?.assets ?? SIX_HOURS
              await this.queue.publishAfter(
                ASSET_METADATA_FETCH_UPDATE,
                { assetId },
                { retryDelay: updateInterval },
                updateInterval
              )
            }
          } else {
            const metadata = subject.metadata
            const existingAssetMetadataHashObj =
              existingAssetMetadataHashes.find(
                (item) => item.assetId.replace(/^\\x/, '') === assetId
              )
            const metadataHash = hash(metadata)
            const updateInterval = this.options?.metadataUpdateInterval?.assets ?? SIX_HOURS
            try {
              if (existingAssetMetadataHashObj?.metadataHash === metadataHash) {
                this.logger.trace(
                  { module: MODULE_NAME, assetId },
                  'Metadata from registry matches local'
                )
              } else {
                this.logger.trace(
                  { module: MODULE_NAME, assetId },
                  'Found metadata in registry'
                )
                await this.hasuraClient.addAssetMetadata({
                  assetId,
                  decimals: metadata.decimals?.value,
                  description: metadata.description?.value,
                  logo: metadata.logo?.value
                    ? processLogoValue(metadata.logo.value)
                    : undefined,
                  name: metadata.name?.value,
                  ticker: metadata.ticker?.value,
                  url: metadata.url?.value,
                  metadataHash
                })
              }
              job.done()
            } catch (error) {
              this.logger.warn(
                { module: MODULE_NAME, assetId },
                `Failed to process metadata job: ${error.message}`
              )
              job.done(error)
            } finally {
              await this.queue.publishAfter(
                ASSET_METADATA_FETCH_UPDATE,
                { assetId },
                {
                  retryDelay: updateInterval,
                  ...(job.name === ASSET_METADATA_FETCH_INITIAL ? { singletonKey: assetId } : {})
                },
                updateInterval
              )
            }
          }
        }
      }
    }
    await this.queue.start()
    await this.queue.subscribe<AssetJobPayload, void>(
      ASSET_METADATA_FETCH_INITIAL,
      {
        batchSize: 10000,
        newJobCheckIntervalSeconds: 5
      },
      subscriptionHandler
    )
    await this.queue.subscribe<AssetJobPayload, void>(
      ASSET_METADATA_FETCH_UPDATE,
      {
        batchSize: 10000,
        newJobCheckIntervalSeconds: 5
      },
      subscriptionHandler
    )
    this.logger.info({ module: MODULE_NAME }, 'Started')
  }

  public async syncMissingMetadata (assetIds: string[]): Promise<void> {
    if (assetIds.length === 0) return
    const total = assetIds.length
    this.logger.info(
      { module: MODULE_NAME, qty: total },
      'Syncing metadata for assets without metadata'
    )
    const FETCH_BATCH_SIZE = 1000
    const LOG_EVERY_N_BATCHES = 100
    let written = 0
    let batchIndex = 0
    for (let i = 0; i < total; i += FETCH_BATCH_SIZE) {
      const chunk = assetIds.slice(i, i + FETCH_BATCH_SIZE)
      const fetchedMetadata = await this.metadataFetchClient.fetch(chunk)
      for (const item of fetchedMetadata) {
        const metadata = item.metadata
        const metadataHash = hash(metadata)
        try {
          await this.hasuraClient.addAssetMetadata({
            assetId: item.subject,
            decimals: metadata.decimals?.value,
            description: metadata.description?.value,
            logo: metadata.logo?.value
              ? processLogoValue(metadata.logo.value)
              : undefined,
            name: metadata.name?.value,
            ticker: metadata.ticker?.value,
            url: metadata.url?.value,
            metadataHash
          })
          written++
        } catch (error) {
          this.logger.warn(
            { module: MODULE_NAME, assetId: item.subject },
            `Failed to write metadata for asset: ${error.message}`
          )
        }
      }
      batchIndex++
      if (batchIndex % LOG_EVERY_N_BATCHES === 0) {
        this.logger.info(
          { module: MODULE_NAME, processed: i + chunk.length, total, written },
          'Metadata sync progress'
        )
      }
    }
    this.logger.info(
      { module: MODULE_NAME, written, total },
      'Metadata sync for existing assets complete'
    )
  }

  public async publishInitialMetadataFetch (assetIds: string[]): Promise<void> {
    if (assetIds.length === 0) return
    this.logger.info(
      { module: MODULE_NAME, qty: assetIds.length },
      'Scheduling initial metadata fetch'
    )
    const THREE_MONTHS = 365
    for (const assetId of assetIds) {
      await this.queue.publish(ASSET_METADATA_FETCH_INITIAL, { assetId }, {
        retryDelay: SIX_HOURS,
        retryLimit: THREE_MONTHS,
        singletonKey: assetId
      })
    }
  }

  public async shutdown () {
    if (this.state !== 'running') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'shutdown')
    }
    this.logger.info({ module: MODULE_NAME }, 'Shutting down')
    await Promise.all([
      this.queue.unsubscribe(ASSET_METADATA_FETCH_INITIAL),
      this.queue.unsubscribe(ASSET_METADATA_FETCH_UPDATE)
    ])
    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Shutdown complete')
  }
}
