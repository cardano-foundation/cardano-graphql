import { CardanoCli } from './CardanoCli'
import fs from 'fs-extra'
import { AssetSupply } from './graphql_types'
import pRetry from 'p-retry'
import util, { DataFetcher } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'

export type LedgerState = {
  accountState: {
    _reserves: string
    _treasury: string
  },
  esNonMyopic: {
    rewardPot: string
  }
}

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  public ledgerStateFetcher: DataFetcher<LedgerState>

  constructor (
    private cardanoCli: CardanoCli,
    pollingInterval: number,
    readonly currentEraFirstSlot: number,
    private logger: Logger = dummyLogger
  ) {
    this.currentEraFirstSlot = currentEraFirstSlot
    this.ledgerStateFetcher = new DataFetcher<LedgerState>(
      'LedgerState',
      this.cardanoCli.getLedgerState,
      pollingInterval,
      this.logger
    )
  }

  public async getTip () {
    const tip = this.cardanoCli.getTip()
    this.logger.debug('getTip', { module: 'CardanoNodeClient', value: tip })
    return tip
  }

  public async initialize () {
    await pRetry(async () => {
      await fs.stat(process.env.CARDANO_NODE_SOCKET_PATH)
      const { slotNo } = await this.getTip()
      if (slotNo < this.currentEraFirstSlot) {
        this.logger.debug('cardano-node tip', { module: 'CardanoNodeClient', value: slotNo })
        this.logger.debug('currentEraFirstSlot', { module: 'CardanoNodeClient', value: this.currentEraFirstSlot })
        this.logger.warn('cardano-node is still synchronizing', { module: 'CardanoNodeClient' })
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
    await this.ledgerStateFetcher.initialize()
  }

  public async isInCurrentEra () {
    return (await this.cardanoCli.getTip()).slotNo >= this.currentEraFirstSlot
  }

  public async shutdown () {
    await this.ledgerStateFetcher.shutdown()
  }
}
