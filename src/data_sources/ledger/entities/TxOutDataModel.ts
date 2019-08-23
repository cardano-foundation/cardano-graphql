import { Column, Entity, PrimaryColumn } from 'typeorm'

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

  @Column('bytea')
  address: string

  @Column('integer')
  value: number
}
