import { DataSource } from 'apollo-datasource'
import alignDataLoaderValues from 'dataloader-values'
import { QueryBlocksArgs, QueryTransactionsArgs } from '../../graphql_types'
import { BlockRepository, TransactionRepository } from './repositories'

export type Config = {
  blocks: BlockRepository
  transactions: TransactionRepository
}

export class Ledger extends DataSource {
  private blockRepository: BlockRepository

  readonly transactionRepository: TransactionRepository

  constructor (config: Config) {
    super()
    this.blockRepository = config.blocks
    this.transactionRepository = config.transactions
  }

  initialize (): void {
    // Placeholder for initializing DataLoaders (optimisation)
  }

  async blocks ({ filter, first }: QueryBlocksArgs) {
    if (filter.ids) {
      return alignDataLoaderValues({
        keys: filter.ids,
        values: await this.blockRepository.byIds(filter.ids, first),
        getKey: ({ id }) => id
      })
    }
    if (filter.numbers) {
      return alignDataLoaderValues({
        keys: filter.numbers,
        values: await this.blockRepository.byNumbers(filter.numbers),
        getKey: ({ number }) => number
      })
    }
  }

  async transactions ({ filter, first }: QueryTransactionsArgs) {
    if (filter.ids) {
      return alignDataLoaderValues({
        keys: filter.ids,
        values: await this.transactionRepository.byIds(filter.ids, first),
        getKey: ({ id }) => id
      })
    }
  }

  blockHeight (): Promise<number> {
    return Promise.resolve(99)
  }
}
