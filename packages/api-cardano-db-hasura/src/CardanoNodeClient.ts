import { Transaction } from './graphql_types'
import fetch from 'cross-fetch'
import pRetry from 'p-retry'
import util, { DataFetcher, errors, ModuleState } from '@cardano-graphql/util'
import {
  ConnectionConfig,
  createConnectionObject,
  createTransactionSubmissionClient,
  getServerHealth,
  ServerHealth
} from '@cardano-ogmios/client'
import { dummyLogger, Logger } from 'ts-log'
import { TransactionSubmissionClient } from '@cardano-ogmios/client/dist/TransactionSubmission'
import { createInteractionContextWithLogger } from './util'

const MODULE_NAME = 'CardanoNodeClient'

export class CardanoNodeClient {
  private txSubmissionClient: TransactionSubmissionClient
  private state: ModuleState
  private serverHealthFetcher: DataFetcher<ServerHealth>

  constructor (
    private prometheusHost: string,
    private prometheusPort: number,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
  }

  public async getTipSlotNo (): Promise<number> {
    const response = await fetch(`http://${this.prometheusHost}:${this.prometheusPort}/metrics`)
    const text = await response.text()
    const match = text.match(/^cardano_node_metrics_slotNum_int\s+(\d+)/m)
    if (!match) {
      throw new Error('cardano_node_metrics_slotNum_int not found in Prometheus metrics')
    }
    return Number(match[1])
  }

  public async initialize (ogmiosConnectionConfig?: ConnectionConfig) {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing')
    this.serverHealthFetcher = this.serverHealthFetcher || new DataFetcher(
      'ServerHealth',
      () => getServerHealth({ connection: createConnectionObject(ogmiosConnectionConfig) }),
      5000, this.logger
    )
    await pRetry(async () => {
      try {
        await this.serverHealthFetcher.initialize()
      } catch (e) {
        this.logger.info('Waiting for Ogmios to be ready...')
        throw e
      }
    }, {
      factor: 1.2,
      retries: 100,
      onFailedAttempt: util.onFailedAttemptFor(
        'Establishing connection to Ogmios server',
        this.logger
      )
    })
    await pRetry(async () => {
      const interactionContext = await createInteractionContextWithLogger(ogmiosConnectionConfig, this.logger, MODULE_NAME, async () => {
        await this.shutdown()
        await this.initialize(ogmiosConnectionConfig)
      })
      this.txSubmissionClient = await createTransactionSubmissionClient(interactionContext)
    }, {
      factor: 1.2,
      retries: 100,
      onFailedAttempt: util.onFailedAttemptFor(
        'Establishing connection to cardano-node',
        this.logger
      )
    })
    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Initialized')
  }

  public async shutdown (): Promise<void> {
    if (this.state !== 'initialized') return
    this.logger.info({ module: MODULE_NAME }, 'Shutting down')
    await this.serverHealthFetcher.shutdown()
    if (this.txSubmissionClient.context.socket.readyState === this.txSubmissionClient.context.socket.OPEN) {
      await this.txSubmissionClient.shutdown()
    }
    this.state = null
    this.logger.info({ module: MODULE_NAME }, 'Shutdown')
  }

  public async submitTransaction (transaction: string): Promise<Transaction['hash']> {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'submitTransaction')
    }
    if (this.serverHealthFetcher.value.networkSynchronization < 0.95) {
      throw new errors.OperationRequiresSyncedNode('submitTransaction')
    }
    const hash = await this.txSubmissionClient.submitTransaction(transaction)
    this.logger.info({ module: MODULE_NAME, hash }, 'submitTransaction')
    return hash
  }
}
