import { AssetSupply, Transaction } from './graphql_types'
import pRetry from 'p-retry'
import util from '@cardano-graphql/util'
import {
  ConnectionConfig,
  createStateQueryClient,
  createTxSubmissionClient,
  StateQueryClient,
  TxSubmissionClient
} from '@cardano-ogmios/client'
import { dummyLogger, Logger } from 'ts-log'
import { getHashOfSignedTransaction } from './util'

const isEraMismatch = (errorMessage: string): boolean =>
  errorMessage.includes('DecoderErrorDeserialiseFailure') ||
  errorMessage.includes('The era of the node and the tx do not match')

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  private stateQueryClient: StateQueryClient
  private txSubmissionClient: TxSubmissionClient

  constructor (
    readonly lastConfiguredMajorVersion: number,
    private logger: Logger = dummyLogger
  ) {}

  public async getTipSlotNo () {
    const tip = await this.stateQueryClient.ledgerTip()
    const slotNo = tip === 'origin' ? 0 : tip.slot
    this.logger.debug({ module: 'CardanoNodeClient', slotNo }, 'getTipSlotNo')
    return slotNo
  }

  public async getProtocolParams () {
    const protocolParams = await this.stateQueryClient.currentProtocolParameters()
    this.logger.debug({ module: 'CardanoNodeClient', protocolParams }, 'getProtocolParams')
    return protocolParams
  }

  public async initialize (ogmiosConnectionConfig?: ConnectionConfig) {
    this.logger.info({ module: 'CardanoNodeClient' }, 'Initializing')
    await pRetry(async () => {
      const options = ogmiosConnectionConfig ? { connection: ogmiosConnectionConfig } : {}
      this.stateQueryClient = await createStateQueryClient(options)
      this.txSubmissionClient = await createTxSubmissionClient(options)
      if (!(await this.isInCurrentEra())) {
        this.logger.warn({ module: 'CardanoNodeClient' }, 'cardano-node is still synchronizing')
        throw new Error()
      }
    }, {
      factor: 1.5,
      retries: 39,
      onFailedAttempt: util.onFailedAttemptFor(
        'Establishing connection to cardano-node and ensuring state is in the expected era',
        this.logger
      )
    })
    this.logger.info({ module: 'CardanoNodeClient' }, 'Initialized')
  }

  private async isInCurrentEra () {
    const { protocolVersion } = await this.getProtocolParams()
    this.logger.debug({
      module: 'CardanoNodeClient',
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
    try {
      await this.txSubmissionClient.submitTx(transaction)
      const hash = getHashOfSignedTransaction(transaction)
      this.logger.info({ module: 'CardanoNodeClient', hash }, 'submitTransaction')
      return hash
    } catch (error) {
      if (!isEraMismatch(error.message)) {
        throw error
      }
    }
  }
}
