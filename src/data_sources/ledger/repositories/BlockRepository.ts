import { EntityRepository, In, Repository } from 'typeorm'
import { BlockDataModel } from '../entities'

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

  static async transform (dataSet: Promise<BlockDataModel[]>) {
    return (await dataSet).map((r: null | BlockDataModel) => {
      if (r === null) return r
      return {
        ...r,
        id: r.hash
      }
    })
  }
}
