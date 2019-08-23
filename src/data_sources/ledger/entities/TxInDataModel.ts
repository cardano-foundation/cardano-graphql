import { Column, Entity, PrimaryColumn, ManyToOne, JoinColumn, OneToOne } from 'typeorm'
import { TxDataModel, TxOutDataModel } from '.';
// import { TxOutDataModel } from './TxOutDataModel'

@Entity('tx_in')
export class TxInDataModel {
  @PrimaryColumn('integer')
  id: number

  @Column({
    name: 'tx_in_id',
    type: 'integer'
  })
  txInId: number

  @Column({
    name: 'tx_out_id',
    type: 'integer'
  })
  txOutId: number

  @Column({
    name: 'tx_out_index',
    type: 'integer'
  })
  txOutIndex: number

  @ManyToOne(_type => TxDataModel, transaction => transaction.inputs)
  @JoinColumn([
    { name: 'tx_in_id' }
  ])
  transaction: TxDataModel

  @OneToOne(_type => TxOutDataModel)
  @JoinColumn([
    { name: 'tx_out_id' },
    { name: 'tx_out_index' }
  ])
  sourceOutput: TxOutDataModel
}
