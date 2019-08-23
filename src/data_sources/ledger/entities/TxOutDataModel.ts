import { Column, Entity, PrimaryColumn, ManyToOne, JoinColumn } from 'typeorm'
import { TxDataModel } from './TxDataModel'
import { BufferTransformer } from '../../lib/BufferTransformer'

@Entity('tx_out')
export class TxOutDataModel {
  @PrimaryColumn('integer')
  id: number

  @Column({
    name: 'tx_id',
    type: 'integer'
  })
  txId: number

  @Column('integer')
  index: number

  @Column({
    type: 'bytea',
    readonly: true,
    transformer: new BufferTransformer()
  })
  address: string

  @Column('integer')
  value: number

  @ManyToOne(_type => TxDataModel, transaction => transaction.outputs)
  @JoinColumn([
    { name: 'tx_id' }
  ])
  transaction: TxDataModel
}
