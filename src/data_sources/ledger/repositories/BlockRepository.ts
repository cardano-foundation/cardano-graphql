import { EntityRepository, In, Repository } from 'typeorm'
import { BlockDataModel } from '../entities'
import { TransactionRepository } from './TransactionRepository'

@EntityRepository(BlockDataModel)
export class BlockRepository extends Repository<BlockDataModel> {
  byNumbers (numbers: BlockDataModel['number'][], limit?: number) {
    return BlockRepository.transform(this.find({
      take: limit || undefined,
      where: { number: In(numbers) }
    }))
  }

  byIds (ids: BlockDataModel['hash'][], limit?: number) {
    return BlockRepository.transform(this.find({
      take: limit || undefined,
      where: { hash: In(ids) }
    }))
  }

  count () {
    return this.createQueryBuilder('block')
      .select('SUM(block.block_no)', 'blockHeight')
      .getRawOne()
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
