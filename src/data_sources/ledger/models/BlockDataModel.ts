import { Column, Entity, OneToMany , PrimaryColumn } from 'typeorm'
import { TxDataModel } from '.'

@Entity()
export class BlockDataModel {
  @PrimaryColumn ('integer')
  id: number

  @Column ({
    type: 'blob',
    transformer: {
      from(value: Buffer) {
        return value.toString('hex')
      },
      to(_value: string) {
        throw new Error('This model is readonly')
      }
    }
  })
  hash: string

  @Column ('integer')
  slot_no: number

  @Column ('integer')
  previous: number

  @Column ({
    type: 'blob',
    transformer: {
      from(value: Buffer) {
        return value.toString('hex')
      },
      to(_value: string) {
        throw new Error('This model is readonly')
      }
    }
  })
  merkel_root: string

  @Column ('integer')
  size: number

  @OneToMany (_type => TxDataModel, tx => tx.block)
  tx: TxDataModel
}
