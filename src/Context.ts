import { Mempool } from './data_sources/mempool'
import { Ledger } from './data_sources/ledger'

export type Context = {
  dataSources: {
    mempool: Mempool
    ledger: Ledger
  }
}
