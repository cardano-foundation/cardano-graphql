import { Column, Entity, PrimaryColumn } from 'typeorm'
// import { TxOutDataModel } from '.'

@Entity()
export class TxInDataModel {
  @PrimaryColumn ('integer')
  id: number

  @Column ('integer')
  tx_in_id: number

  @Column ('integer')
  tx_out_id: number

  @Column ('integer')
  tx_out_index: number

  // @OneToOne (_type => TxOutDataModel)
  // @JoinColumn()
}
