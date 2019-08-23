import { Column, Entity, OneToMany, PrimaryColumn } from 'typeorm'
import { TxDataModel } from './TxDataModel'
import { BufferTransformer } from '../../lib/BufferTransformer'

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

  @Column('integer')
  previous: number

  @Column({
    name: 'merkel_root',
    type: 'bytea',
    transformer: new BufferTransformer()
  })
  merkelRoot: string

  @Column('integer')
  size: number

  @OneToMany(_type => TxDataModel, tx => tx.block)
  tx: TxDataModel
}
