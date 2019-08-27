import { Column, Entity, PrimaryColumn, OneToMany } from 'typeorm'
import { BufferTransformer } from '../../lib/BufferTransformer'
import { TxDataModel } from './TxDataModel'

@Entity('block')
export class BlockDataModel {
  @PrimaryColumn('integer')
  id: number

  @Column({
    type: 'bytea',
    transformer: new BufferTransformer()
  })
  hash: string

  @Column({
    name: 'slot_no',
    type: 'integer'
  })
  slotNo: number

  @Column({
    name: 'block_no',
    type: 'integer'
  })
  number: number

  @Column({
    name: 'previous',
    type: 'integer'
  })
  previousBlockNo: number

  @Column({
    name: 'merkel_root',
    type: 'bytea',
    transformer: new BufferTransformer()
  })
  merkelRootHash: string

  @Column('integer')
  size: number

  @OneToMany(_type => TxDataModel, tx => tx.block)
  transactions: TxDataModel[]
}
