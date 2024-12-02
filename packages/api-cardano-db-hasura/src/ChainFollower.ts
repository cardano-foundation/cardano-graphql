import {
  createChainSynchronizationClient,
  Schema
} from '@cardano-ogmios/client'
import pRetry from 'p-retry'
import { Config } from './Config'
import util, { assetFingerprint, errors, RunnableModuleState } from '@cardano-graphql/util'
import PgBoss from 'pg-boss'
import { dummyLogger, Logger } from 'ts-log'
import { createInteractionContextWithLogger } from './util'
import { PointOrOrigin, BlockPraos, BlockBFT, Tip, Origin } from '@cardano-ogmios/schema'
import { HasuraBackgroundClient } from './HasuraBackgroundClient'
import { DbConfig } from './typeAliases'
import { ChainSynchronizationClient } from '@cardano-ogmios/client/dist/ChainSynchronization'

const MODULE_NAME = 'ChainFollower'

export class ChainFollower {
  private chainSyncClient: ChainSynchronizationClient
  private queue: PgBoss
  private state: RunnableModuleState
  private cacheAssets : { assetId: string; assetName: string; firstAppearedInSlot: number; fingerprint: string; policyId: string; }[]
  private cacheTimer : number

  constructor (
    readonly hasuraClient: HasuraBackgroundClient,
    private logger: Logger = dummyLogger,
    private queueConfig: DbConfig
  ) {
    this.state = null
    this.cacheAssets = []
    this.cacheTimer = Date.now()
  }

  public async initialize (ogmiosConfig: Config['ogmios'], getMostRecentPoint: () => Promise<PointOrOrigin[]>) {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing')
    this.queue = new PgBoss({
      application_name: 'cardano-graphql',
      ...this.queueConfig
    })
    this.logger.info({ module: MODULE_NAME }, 'Connecting to queue')
    await pRetry(async () => {
      const context = await createInteractionContextWithLogger(ogmiosConfig, this.logger, MODULE_NAME, async () => {
        await this.shutdown()
        await this.initialize(ogmiosConfig, getMostRecentPoint)
        await this.start(await getMostRecentPoint())
      })
      this.chainSyncClient = await createChainSynchronizationClient(
        context,
        {
          rollBackward: async ({ point, tip }, requestNext) => {
            if (point !== 'origin') {
              this.logger.info(
                { module: MODULE_NAME, tip, rollbackPoint: point }, 'Rolling back'
              )
              const deleteResult = await this.hasuraClient.deleteAssetsAfterSlot(point.slot.toString())
              this.logger.info({ module: MODULE_NAME }, `Deleted ${deleteResult} assets`)
            } else {
              this.logger.info({ module: MODULE_NAME }, 'Rolling back to genesis')
              const deleteResult = await this.hasuraClient.deleteAssetsAfterSlot('0')
              this.logger.info({ module: MODULE_NAME }, `Deleted ${deleteResult} assets`)
            }
            requestNext()
          },
          rollForward: async ({ block, tip }, requestNext) => {
            try {
              let b
              switch (block.type) {
                case 'praos':
                  b = block as BlockPraos
                  break
                case 'bft':
                  b = block as BlockBFT
                  break
                case 'ebb': // No transaction in there
                  return
              }
              if (b !== undefined && b.transactions !== undefined) {
                for (const tx of b.transactions) {
                  if (tx.mint !== undefined) {
                    for (const entry of Object.entries(tx.mint)) {
                      const policyId = entry[0]
                      const assetNames = Object.keys(entry[1])
                      for (const assetName of assetNames) {
                        await this.saveAsset(policyId, assetName, b, tip)
                      }
                    }
                  }
                }
              }
            } catch (e) {
              console.log(e)
            } finally {
              requestNext()
            }
          }
        }
      )
    }, {
      factor: 1.2,
      retries: 100,
      onFailedAttempt: util.onFailedAttemptFor(
        'Establishing connection to cardano-node chain-sync',
        this.logger
      )
    })
    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Initialized')
  }

  async saveAsset (policyId: string, assetName: string | undefined, b: BlockPraos | BlockBFT, tip: Tip | Origin) {
    const assetId = `${policyId}${assetName !== undefined ? assetName : ''}`
    const asset = {
      assetId,
      assetName,
      firstAppearedInSlot: b.slot,
      fingerprint: assetFingerprint(policyId, assetName),
      policyId
    }
    // introducing a caching to speed things up. The GraphQL insertAssets takes a lot of time.
    // Saving when > 1000 assets in the cache or every minute
    this.cacheAssets.push(asset)
    let isTip = false
    try {
      isTip = (tip as Tip).slot === b.slot
    } catch (e) {
      this.logger.debug({ module: MODULE_NAME }, 'Sync is not at tip. Using a cache to save Assets every minute to increase catching up speed.')
    }
    if (isTip || this.cacheAssets.length > 1000 || (Date.now() - this.cacheTimer) / 1000 > 60) {
      this.cacheTimer = Date.now() // resetting the timer
      const response = await this.hasuraClient.insertAssets(this.cacheAssets)
      this.cacheAssets = []
      response.insert_assets.returning.forEach((asset: { assetId: string }) => {
        const SIX_HOURS = 21600
        const THREE_MONTHS = 365
        this.queue.publish('asset-metadata-fetch-initial', { assetId: asset.assetId.replace('\\x', '') }, {
          retryDelay: SIX_HOURS,
          retryLimit: THREE_MONTHS
        })
      })
    }
  }

  public async start (points: Schema.PointOrOrigin[]) {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'start')
    }
    this.logger.info({ module: MODULE_NAME }, 'Starting from ' + JSON.stringify(points))
    await this.queue.start()
    await this.chainSyncClient.resume(points)
    this.state = 'running'
    this.logger.info({ module: MODULE_NAME }, 'Started')
  }

  public async shutdown () {
    if (this.state !== 'running') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'shutdown')
    }
    this.logger.info({ module: MODULE_NAME }, 'Shutting down')
    await this.queue.stop()
    if (this.chainSyncClient.context.socket.readyState === this.chainSyncClient.context.socket.OPEN) {
      await this.chainSyncClient.shutdown()
    }
    this.state = null
    this.logger.info(
      { module: MODULE_NAME },
      'Shutdown complete')
  }
}
