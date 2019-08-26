import { EntityRepository, In, Repository } from 'typeorm'
import { BlockDataModel } from '../entities'
import { TransactionRepository } from './TransactionRepository'

@EntityRepository(BlockDataModel)
export class BlockRepository extends Repository<BlockDataModel> {
  static standardRelations = [
    'transactions',
    'transactions.inputs',
    'transactions.inputs.sourceOutput',
    'transactions.inputs.sourceOutput.transaction',
    'transactions.outputs'
  ]

  byNumbers (numbers: BlockDataModel['number'][], limit?: number) {
    return BlockRepository.transform(this.find({
      take: limit || undefined,
      where: { number: In(numbers) },
      relations: BlockRepository.standardRelations
    }))
  }

  byIds (ids: BlockDataModel['hash'][], limit?: number) {
    return BlockRepository.transform(this.find({
      take: limit || undefined,
      where: { hash: In(ids) },
      relations: BlockRepository.standardRelations
    }))
  }

  async count () {
    const { sum: countAsString } = await this.createQueryBuilder('block')
      .select('SUM(block.block_no)')
      .getRawOne()

    return parseInt(countAsString)
  }

  static async transform (dataSet: Promise<BlockDataModel[]>) {
    return (await dataSet).map((r: BlockDataModel) => {
      return {
        ...r,
        id: r.hash,
        transactions: r.transactions.map(TransactionRepository.castTransaction)
      }
    })
  }
}
