import {
  ChainSync,
  createChainSyncClient,
  createInteractionContext,
  Schema
} from '@cardano-ogmios/client'
import { errors, RunnableModuleState } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'

const MODULE_NAME = 'ChainFollower'

interface Config {
  cardanoNodeConfigPath: string;
  db: {
    database: string;
    host: string;
    password: string;
    port: number;
    user: string;
  };
  ogmios?: {
    host?: string;
    port?: number;
  };
}

export class ChainFollower {
  private chainSyncClient: ChainSync.ChainSyncClient;
  private state: RunnableModuleState;

  constructor (private logger: Logger = dummyLogger) {
    this.state = null
  }

  public async initialize (ogmiosConfig: Config['ogmios']) {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing')

    const context = await createInteractionContext(
      this.logger.error,
      (code, reason) => {
        this.logger.error({ module: MODULE_NAME, code }, reason)
      },
      {
        connection: ogmiosConfig,
        interactionType: 'LongRunning'
      }
    )

    this.chainSyncClient = await createChainSyncClient(context, {
      rollBackward: async ({ point, tip }, requestNext) => {
        if (point !== 'origin') {
          this.logger.info(
            { module: MODULE_NAME, tip, rollbackPoint: point },
            'Rolling back'
          )
          // const deleteResult = await this.dgraphClient.deleteDataAfterSlot(point.slot);
          this.logger.info(
            { module: MODULE_NAME },
            'Deleted data'
          )
        } else {
          this.logger.info(
            { module: MODULE_NAME },
            'Rolling back to genesis'
          )
          // const deleteResult = await this.dgraphClient.deleteDataAfterSlot(0);
          this.logger.info(
            { module: MODULE_NAME },
            'Deleted data'
          )
        }
        requestNext()
      },
      rollForward: async ({ block }, requestNext) => {
        this.logger.info({ BLOCK: block }, 'Rolling forward')

        // todo

        requestNext()
      }
    })

    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Initialized')
  }

  public async start (points: Schema.PointOrOrigin[]) {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'start')
    }
    this.logger.info({ module: MODULE_NAME }, 'Starting')
    await this.chainSyncClient.startSync(points)
    this.logger.info({ module: MODULE_NAME }, 'Started')
  }

  public async shutdown () {
    if (this.state !== 'running') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'shutdown')
    }
    this.logger.info({ module: MODULE_NAME }, 'Shutting down')
    await this.chainSyncClient.shutdown()
    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Shutdown complete')
  }
}
