import { AssetSupply, Transaction } from './graphql_types'
import pRetry from 'p-retry'
import util, { DataFetcher, errors, ModuleState } from '@cardano-graphql/util'
import {
  ConnectionConfig,
  createConnectionObject,
  createStateQueryClient,
  createTxSubmissionClient,
  getServerHealth,
  ServerHealth,
  // Schema,
  StateQuery,
  TxSubmission
} from '@cardano-ogmios/client'
import { dummyLogger, Logger } from 'ts-log'
import { createInteractionContextWithLogger } from './util'

const MODULE_NAME = 'CardanoNodeClient'

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  private stateQueryClient: StateQuery.StateQueryClient
  private txSubmissionClient: TxSubmission.TxSubmissionClient
  private state: ModuleState
  private serverHealthFetcher: DataFetcher<ServerHealth>

  constructor (
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
      () => getServerHealth({ connection: createConnectionObject(ogmiosConnectionConfig) }),
      30000, this.logger
    )
    await pRetry(async () => {
      await this.serverHealthFetcher.initialize()
      this.stateQueryClient = await createStateQueryClient(
        await createInteractionContextWithLogger(ogmiosConnectionConfig, this.logger, MODULE_NAME)
      )
      this.txSubmissionClient = await createTxSubmissionClient(
        await createInteractionContextWithLogger(ogmiosConnectionConfig, this.logger, MODULE_NAME)
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
    const hash = await this.txSubmissionClient.submitTx(transaction)
    this.logger.info({ module: MODULE_NAME, hash }, 'submitTransaction')
    return hash
  }
}
