import { Ledger } from './data_sources/ledger'

export type Context = {
  dataSources: {
    ledger: Ledger
  }
}
