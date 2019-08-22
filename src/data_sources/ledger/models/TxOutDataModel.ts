import { Column, Entity, PrimaryColumn } from 'typeorm'

@Entity()
export class TxOutDataModel {
  @PrimaryColumn('integer')
  id: number

  @Column('integer')
  tx_id: number

  @Column('integer')
  index: number

  @Column('blob')
  address: string

  @Column('integer')
  value: number
}
