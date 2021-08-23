import { AssetSupply, Transaction } from './graphql_types'
import pRetry from 'p-retry'
import util, { DataFetcher, errors, ModuleState } from '@cardano-graphql/util'
import {
  ConnectionConfig, createConnectionObject, createInteractionContext,
  createStateQueryClient,
  createTxSubmissionClient,
  getServerHealth,
  ServerHealth,
  // Schema,
  StateQuery,
  TxSubmission
} from '@cardano-ogmios/client'
import { dummyLogger, Logger } from 'ts-log'
import { getHashOfSignedTransaction } from './util'

const MODULE_NAME = 'CardanoNodeClient'

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  private stateQueryClient: StateQuery.StateQueryClient
  private txSubmissionClient: TxSubmission.TxSubmissionClient
  private state: ModuleState
  private serverHealthFetcher: DataFetcher<ServerHealth>

  constructor (
    readonly lastConfiguredMajorVersion: number, // Todo: Depreciate
    private logger: Logger = dummyLogger
  ) {
    this.state = null
  }

  public async getTipSlotNo () {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'getTipSlotNo')
    }
    const tip = await this.stateQueryClient.ledgerTip()
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
    this.serverHealthFetcher = new DataFetcher(
      'ServerHealth',
      () => getServerHealth(createConnectionObject(ogmiosConnectionConfig)),
      30000, this.logger
    )
    await pRetry(async () => {
      await this.serverHealthFetcher.initialize()
      this.stateQueryClient = await createStateQueryClient(
        await createInteractionContext(
          (error) => {
            this.logger.error({ module: MODULE_NAME, error: error.name }, error.message)
          },
          this.logger.info,
          {
            connection: ogmiosConnectionConfig,
            interactionType: 'LongRunning'
          }
        )
      )
      this.txSubmissionClient = await createTxSubmissionClient(
        await createInteractionContext(
          (error) => {
            this.logger.error({ module: MODULE_NAME, error: error.name }, error.message)
          },
          this.logger.info,
          {
            connection: ogmiosConnectionConfig,
            interactionType: 'LongRunning'
          }
        )
      )
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
    await Promise.all([
      this.serverHealthFetcher.shutdown,
      this.stateQueryClient.shutdown,
      this.txSubmissionClient.shutdown
    ])
  }

  public async submitTransaction (transaction: string): Promise<Transaction['hash']> {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'submitTransaction')
    }
    if (this.serverHealthFetcher.value.networkSynchronization < 0.95) {
      throw new errors.OperationRequiresSyncedNode('submitTransaction')
    }
    await this.txSubmissionClient.submitTx(transaction)
    const hash = getHashOfSignedTransaction(transaction)
    this.logger.info({ module: MODULE_NAME, hash }, 'submitTransaction')
    return hash
  }
}
