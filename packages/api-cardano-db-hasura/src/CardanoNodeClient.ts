import { CardanoCli } from './CardanoCli'
import fs from 'fs-extra'
import { AssetSupply } from './graphql_types'
import pRetry from 'p-retry'
import util, { DataFetcher } from '@cardano-graphql/util'

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
  private cardanoCli: CardanoCli
  readonly currentEraFirstSlot: number
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  public ledgerStateFetcher: DataFetcher<LedgerState>

  constructor (
    cardanoCli: CardanoCli,
    pollingInterval: number,
    currentEraFirstSlot: number
  ) {
    this.cardanoCli = cardanoCli
    this.currentEraFirstSlot = currentEraFirstSlot
    this.ledgerStateFetcher = new DataFetcher<LedgerState>(
      this.cardanoCli.getLedgerState,
      pollingInterval
    )
  }

  public async initialize () {
    await pRetry(async () => {
      await fs.stat(process.env.CARDANO_NODE_SOCKET_PATH)
      const { slotNo } = await this.cardanoCli.getTip()
      if (slotNo < this.currentEraFirstSlot) {
        throw new Error('cardano-node is still synchronizing')
      }
    }, {
      factor: 1.5,
      retries: 39,
      onFailedAttempt: util.onFailedAttemptFor(
        'Establishing connection to cardano-node and ensuring state is in the expected era'
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
