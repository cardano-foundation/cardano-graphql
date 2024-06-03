import { AssetSupply, Transaction } from './graphql_types'
import pRetry from 'p-retry'
import util, { DataFetcher, errors, ModuleState } from '@cardano-graphql/util'
import {
  ConnectionConfig,
  createConnectionObject, createLedgerStateQueryClient, createTransactionSubmissionClient,
  getServerHealth,
  ServerHealth
} from '@cardano-ogmios/client'
import { dummyLogger, Logger } from 'ts-log'
import { LedgerStateQueryClient } from '@cardano-ogmios/client/dist/LedgerStateQuery'
import { TransactionSubmissionClient } from '@cardano-ogmios/client/dist/TransactionSubmission'
import { createInteractionContextWithLogger } from './util'

const MODULE_NAME = 'CardanoNodeClient'

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  private stateQueryClient: LedgerStateQueryClient
  private txSubmissionClient: TransactionSubmissionClient
  private state: ModuleState
  private serverHealthFetcher: DataFetcher<ServerHealth>

  constructor (
    private logger: Logger = dummyLogger
  ) {
    this.state = null
  }

  public async getTipSlotNo () {
    this.logger.info({ module: MODULE_NAME }, this.state)
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'getTipSlotNo')
    }
    const tip = await this.stateQueryClient.networkTip()
    this.logger.info({ module: MODULE_NAME, tip }, tip)
    const slotNo = tip === 'origin' ? 0 : tip.slot
    this.logger.debug({ module: MODULE_NAME, slotNo }, 'getTipSlotNo')
    return slotNo
  }

  // Todo: Include in Graph
  // public async getProtocolParams (): Promise<Schema.ProtocolParametersShelley> {
  //   if (this.state !== 'initialized') {
  //     throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'getProtocolParams')
  //   }
  //   const protocolParams = await this.stateQueryClient.currentProtocolParameters()
  //   this.logger.debug({ module: MODULE_NAME, protocolParams }, 'getProtocolParams')
  //   return protocolParams
  // }

  public async initialize (ogmiosConnectionConfig?: ConnectionConfig) {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing. This can take a few minutes...')
    this.serverHealthFetcher = this.serverHealthFetcher || new DataFetcher(
      'ServerHealth',
      () => getServerHealth({ connection: createConnectionObject(ogmiosConnectionConfig) }),
      5000, this.logger
    )
    await this.serverHealthFetcher.initialize()
    await pRetry(async () => {
      const serverHealth = await getServerHealth({ connection: createConnectionObject(ogmiosConnectionConfig) })
      this.logger.info(serverHealth)
      if (serverHealth === undefined) {
        throw new Error()
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
      this.stateQueryClient = await createLedgerStateQueryClient(interactionContext)
      let tip
      try {
        tip = await this.stateQueryClient.ledgerTip()
      } catch (e) {
        this.logger.error({ module: MODULE_NAME }, 'Querying ledger tip not yet available. Wait for later epoch. Ogmios Error Message: ' + e.message)
        throw e
      }

      this.logger.info({ module: MODULE_NAME }, tip)
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
    if (this.stateQueryClient.context.socket.readyState === this.stateQueryClient.context.socket.OPEN) {
      await this.stateQueryClient.shutdown()
    }
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
