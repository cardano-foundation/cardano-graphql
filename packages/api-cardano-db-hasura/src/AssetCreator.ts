import { RunnableModuleState } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'
import { HasuraBackgroundClient } from './HasuraBackgroundClient'

const MODULE_NAME = 'AssetCreator'

export class AssetCreator {
  private state: RunnableModuleState

  constructor (
    // private dbConfig : DbConfig,
    private logger: Logger = dummyLogger,
    private hasuraBackgroundClient : HasuraBackgroundClient
  ) {
    this.state = null
    this.logger.info({ module: MODULE_NAME }, 'Constructor')
  }

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing')
    await this.hasuraBackgroundClient.applySchemaAndMetadata()
    const maxSlot = await this.hasuraBackgroundClient.getMaximumSlotFromAssets()
    const tokenMintsAfterSlot = await this.hasuraBackgroundClient.getTokenMintsAfterSlot(maxSlot)
    await this.hasuraBackgroundClient.insertAssets(tokenMintsAfterSlot)
    this.logger.info({ module: MODULE_NAME }, 'Saved Assets')
  }
}
