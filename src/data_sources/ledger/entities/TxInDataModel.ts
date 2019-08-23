import { Column, Entity, JoinColumn, OneToOne, PrimaryColumn } from 'typeorm'
import { TxOutDataModel } from './TxOutDataModel'

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

  @OneToOne(_type => TxOutDataModel)
  @JoinColumn([
    { referencedColumnName: 'txId' },
    { referencedColumnName: 'index' }
  ])
  sourceTxOut: TxOutDataModel
}
