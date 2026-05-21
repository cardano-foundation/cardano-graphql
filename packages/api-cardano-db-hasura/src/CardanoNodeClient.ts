import { Transaction } from './graphql_types'
import fetch from 'cross-fetch'
import { dummyLogger, Logger } from 'ts-log'

const MODULE_NAME = 'CardanoNodeClient'

export class CardanoNodeClient {
  constructor (
    private prometheusHost: string,
    private prometheusPort: number,
    private submitApiHost: string,
    private submitApiPort: number,
    private logger: Logger = dummyLogger
  ) {}

  public async getTipSlotNo (): Promise<number> {
    const response = await fetch(`http://${this.prometheusHost}:${this.prometheusPort}/metrics`)
    const text = await response.text()
    const match = text.match(/^cardano_node_metrics_slotNum_int\s+(\d+)/m)
    if (!match) {
      throw new Error('cardano_node_metrics_slotNum_int not found in Prometheus metrics')
    }
    return Number(match[1])
  }

  public async submitTransaction (transaction: string): Promise<Transaction['hash']> {
    const url = `http://${this.submitApiHost}:${this.submitApiPort}/api/submit/tx`
    const response = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/cbor' },
      body: Buffer.from(transaction, 'hex')
    })
    if (!response.ok) {
      const errorBody = await response.text()
      throw new Error(`Transaction submission failed: ${errorBody}`)
    }
    const hash = await response.json() as string
    this.logger.info({ module: MODULE_NAME, hash }, 'submitTransaction')
    return hash
  }
}
