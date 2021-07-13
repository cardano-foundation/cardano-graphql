import { AssetSupply, Transaction } from './graphql_types'
import pRetry from 'p-retry'
import util, { errors, ModuleState } from '@cardano-graphql/util'
import {
  ConnectionConfig,
  createStateQueryClient,
  createTxSubmissionClient,
  Schema,
  StateQueryClient,
  TxSubmissionClient
} from '@cardano-ogmios/client'
import { dummyLogger, Logger } from 'ts-log'
import { getHashOfSignedTransaction } from './util'

const MODULE_NAME = 'CardanoNodeClient'

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  private stateQueryClient: StateQueryClient
  private txSubmissionClient: TxSubmissionClient
  private state: ModuleState

  constructor (
    readonly lastConfiguredMajorVersion: number,
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

  public async getProtocolParams (): Promise<Schema.ProtocolParametersShelley> {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'getProtocolParams')
    }
    if (!(await this.isInCurrentEra())) {
      throw new errors.OperationRequiresNodeInCurrentEra('getProtocolParams')
    }
    const protocolParams = await this.stateQueryClient.currentProtocolParameters()
    this.logger.debug({ module: MODULE_NAME, protocolParams }, 'getProtocolParams')
    return protocolParams
  }

  public async initialize (ogmiosConnectionConfig?: ConnectionConfig) {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing. This can take a few minutes...')
    await pRetry(async () => {
      const options = ogmiosConnectionConfig ? { connection: ogmiosConnectionConfig } : {}
      this.stateQueryClient = await createStateQueryClient(
        this.logger.error,
        (code, reason) => {
          this.logger.error({ module: MODULE_NAME, code }, reason)
        },
        options
      )
      this.txSubmissionClient = await createTxSubmissionClient(
        this.logger.error,
        (code, reason) => {
          this.logger.error({ module: MODULE_NAME, code }, reason)
        },
        options
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

  private async isInCurrentEra () {
    const { protocolVersion } = await this.getProtocolParams()
    this.logger.debug({
      module: MODULE_NAME,
      currentProtocolVersion: protocolVersion,
      lastConfiguredMajorVersion: this.lastConfiguredMajorVersion
    }, 'Comparing current protocol params with last known major version from cardano-node config')
    return protocolVersion.major >= this.lastConfiguredMajorVersion
  }

  public async shutdown (): Promise<void> {
    await Promise.all([
      this.stateQueryClient.release,
      this.txSubmissionClient.shutdown
    ])
  }

  public async submitTransaction (transaction: string): Promise<Transaction['hash']> {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'submitTransaction')
    }
    await this.txSubmissionClient.submitTx(transaction)
    const hash = getHashOfSignedTransaction(transaction)
    this.logger.info({ module: MODULE_NAME, hash }, 'submitTransaction')
    return hash
  }
}
