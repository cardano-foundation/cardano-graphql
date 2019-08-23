import { DataSource } from 'apollo-datasource'
// import * as DataLoader from 'dataloader'
import { In, Repository } from 'typeorm'
// import alignDataLoaderValues from 'dataloader-values'
// import { Block, Epoch, Transaction, TransactionOutput } from '../../graphql_types'
import { TxDataModel } from './entities'
import { QueryTransactionsArgs } from '../../graphql_types'

export type Config = {
  transactions: Repository<TxDataModel>
}

export class Ledger extends DataSource {
  private transactionRepository: Repository<TxDataModel>

  constructor (config: Config) {
    super()
    this.transactionRepository = config.transactions
  }

  initialize (): void {
    // this.transactionsLoader = new DataLoader<Block['number'], Transaction>(async (numbers: Block['number'][]) => {
    //   return alignDataLoaderValues({
    //     keys: numbers,
    //     values: await this.transactionRepository.find(numbers),
    //     getKey: ({ hash }) => hash
    //   })
    // })
  }

  transactions(args: QueryTransactionsArgs) {
    const whereConditions = []
    if (args.filter) {
      const { filter } = args
      if(filter.ids) whereConditions.push({ hash: In(filter.ids) })
      if(filter.blockNumbers) whereConditions.push({ block: In(filter.blockNumbers) })
    }
    return this.transactionRepository.find({
      take: args.first ? args.first : undefined,
      where: whereConditions,
      relations: ['txIn', 'txOut']
    })
  }

  blockHeight (): Promise<number> {
    return Promise.resolve(99)
  }
}
