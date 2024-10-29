import { RunnableModuleState } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'
import { HasuraBackgroundClient } from './HasuraBackgroundClient'
import { AssetWithoutTokens, DbConfig } from './typeAliases'
import PgBoss from 'pg-boss'
import pRetry from 'p-retry'

const MODULE_NAME = 'AssetCreator'
const SIX_HOURS = 21600
const THREE_MONTHS = 365

export class AssetCreator {
  private state: RunnableModuleState
  private queue: PgBoss

  constructor (
    // private dbConfig : DbConfig,
    private logger: Logger = dummyLogger,
    private hasuraBackgroundClient : HasuraBackgroundClient,
    private queueConfig: DbConfig

  ) {
    this.state = null
    this.logger.info({ module: MODULE_NAME }, 'Constructor')
  }

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing')
    this.queue = new PgBoss({
      application_name: 'cardano-graphql',
      ...this.queueConfig
    })
    await this.hasuraBackgroundClient.applySchemaAndMetadata()
  }

  public async delay (ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms))
  }

  public async start () {
    if (this.state !== 'initializing') return
    this.state = 'running'
    this.logger.info({ module: MODULE_NAME }, 'Starting')
    await this.queue.start()
    await pRetry(async () => {
      while (true) {
        const assetsSavedCount = await this.saveAssets()
        if (assetsSavedCount === 0) {
          await this.delay(60000)// then wait a minute, for new assets to be minted
        }
      }
    },
    {
      onFailedAttempt: async (error) => {
        this.logger.error({ module: MODULE_NAME }, error)
      },
      retries: 100
    }
    )
  }

  private async saveAssets () {
    const tokenMintsAfterSlot = await this.hasuraBackgroundClient.getTokenMintsForMaxSlot()
    const tokensFiltered: AssetWithoutTokens[] = []
    const reversed = tokenMintsAfterSlot.reverse()
    while (reversed.length > 0) {
      const asset = reversed.pop()
      const index = reversed.findIndex((t) => {
        if (t.assetId === asset.assetId) {
          return true
        }
        return false
      })
      // Asset wasn't processed yet
      if (index === -1 && !this.hasuraBackgroundClient.hasAsset(asset.assetId)) {
        const assetId = asset.assetId
        await this.queue.publish('asset-metadata-fetch-initial', { assetId }, {
          retryDelay: SIX_HOURS,
          retryLimit: THREE_MONTHS
        })
        tokensFiltered.push(asset)
      }
    }
    await this.hasuraBackgroundClient.insertAssets(tokensFiltered)
    if (tokensFiltered.length > 0) {
      this.logger.info({ module: MODULE_NAME }, 'Saved Assets ' + tokensFiltered.length)
    }
    return tokensFiltered.length
  }
}
