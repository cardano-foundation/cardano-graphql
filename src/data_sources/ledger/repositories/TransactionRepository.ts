import { EntityRepository, In, Repository } from 'typeorm'
import { Transaction } from '../../../graphql_types'
import { TxDataModel } from '../entities'

@EntityRepository(TxDataModel)
export class TransactionRepository extends Repository<TxDataModel> {
  byBlockNumbers (numbers: number[], limit: number) {
    return TransactionRepository.transform(this.find({
      take: limit || undefined,
      where: { block: In(numbers) },
      relations: TransactionRepository.standardRelations
    }))
  }

  byIds (ids: TxDataModel['hash'][], limit?: number) {
    return TransactionRepository.transform(this.find({
      take: limit || undefined,
      where: { hash: In(ids) },
      relations: TransactionRepository.standardRelations
    }))
  }

  static standardRelations = [
    'outputs',
    'inputs',
    'inputs.sourceOutput',
    'inputs.sourceOutput.transaction'
  ]

  static async transform (dataSet: Promise<TxDataModel[]>) {
    return (await dataSet).map((data: null | TxDataModel) => {
      if (data === null) return data
      delete data.id
      const transformedData = {
        ...data,
        id: data.hash,
        // Even though the model defines some entity properties as numbers, they're coming back as string
        // TypeORM with MySQL has a connection option that covers this, but it's not on the PostgresConnectionOptions
        // https://typeorm.io/#/connection-options/mysql--mariadb-connection-options
        blockNo: parseInt(String(data.blockNo)),
        fee: parseInt(String(data.fee)),
        inputs: data.inputs.map(input => {
          return {
            sourceTxId: input.sourceOutput.transaction.hash,
            sourceTxIndex: input.sourceOutput.index,
            value: parseInt(String(input.sourceOutput.value)),
            address: input.sourceOutput.address
          }
        }),
        outputs: data.outputs.map(output => {
          delete output.id
          return {
            ...output,
            // This output is created by this transaction, so we know
            // this is the correct hash.
            txId: data.hash,
            value: parseInt(String(output.value))
          }
        })
      }
      delete transformedData.hash
      return transformedData as Transaction
    })
  }
}
