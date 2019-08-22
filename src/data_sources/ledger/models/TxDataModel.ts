import { Column, Entity, OneToMany , PrimaryColumn } from 'typeorm'
import { TxInDataModel } from '.'

@Entity()
export class TxDataModel {
  @PrimaryColumn ('integer')
  id: number

  @Column ({
    type: 'blob',
    transformer: {
      from(value: Buffer) {
        return value.toString('hex')
      },
      to(_value: string) {
        throw new Error('Write not supported')
      }
    }
  })
  hash: string

  @Column ('integer')
  block: number

  @Column ('integer')
  fee: number

  @OneToMany (_type => TxInDataModel, txIn => txIn.tx_in_id)
  txIn: TxInDataModel
}
