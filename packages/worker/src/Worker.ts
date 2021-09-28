import { errors, RunnableModuleState } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'
import { QueueClient } from './Queue'

const MODULE_NAME = 'Worker'

export class Worker {
  private state: RunnableModuleState

  constructor (
    // @ts-ignore
    private queue: QueueClient,
    private logger: Logger = dummyLogger
  ) {
    this.state = 'initialized'
  }

  public async start () {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'start')
    }

    this.logger.info({ module: MODULE_NAME }, 'Starting')

    // todo

    this.logger.info({ module: MODULE_NAME }, 'Started')
  }

  public async shutdown () {
    if (this.state !== 'running') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'shutdown')
    }

    this.logger.info({ module: MODULE_NAME }, 'Shutting down')

    // todo

    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Shutdown complete')
  }
}
