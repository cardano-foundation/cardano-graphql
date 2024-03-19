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
import { PointOrOrigin, BlockPraos } from '@cardano-ogmios/schema'
import { HasuraBackgroundClient } from './HasuraBackgroundClient'
import { DbConfig } from './typeAliases'
import { ChainSynchronizationClient } from '@cardano-ogmios/client/dist/ChainSynchronization'

const MODULE_NAME = 'ChainFollower'

export class ChainFollower {
  private chainSyncClient: ChainSynchronizationClient
  private queue: PgBoss
  private state: RunnableModuleState

  constructor (
    readonly hasuraClient: HasuraBackgroundClient,
    private logger: Logger = dummyLogger,
    private queueConfig: DbConfig
  ) {
    this.state = null
  }

  public async initialize (ogmiosConfig: Config['ogmios'], getMostRecentPoint: () => Promise<PointOrOrigin[]>) {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing')
    this.queue = new PgBoss({
      application_name: 'cardano-graphql',
      ...this.queueConfig
    })
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
          rollForward: async ({ block }, requestNext) => {
            const b = block as BlockPraos
            if (b !== undefined && b.transactions !== undefined) {
              for (const tx of b.transactions) {
                if (tx.mint !== undefined) {
                  for (const entry of Object.entries(tx.mint.assets)) {
                    const [policyId, assetName] = entry[0].split('.')
                    const assetId = `${policyId}${assetName !== undefined ? assetName : ''}`
                    if (!(await this.hasuraClient.hasAsset(assetId))) {
                      const asset = {
                        assetId,
                        assetName,
                        firstAppearedInSlot: b.slot,
                        fingerprint: assetFingerprint(policyId, assetName),
                        policyId
                      }
                      await this.hasuraClient.insertAssets([asset])
                      const SIX_HOURS = 21600
                      const THREE_MONTHS = 365
                      await this.queue.publish('asset-metadata-fetch-initial', { assetId }, {
                        retryDelay: SIX_HOURS,
                        retryLimit: THREE_MONTHS
                      })
                    }
                  }
                }
              }
            }
            requestNext()
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

  public async start (points: Schema.PointOrOrigin[]) {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'start')
    }
    this.logger.info({ module: MODULE_NAME }, 'Starting')
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
