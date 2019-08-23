import { Column, Entity, OneToMany, PrimaryColumn } from 'typeorm'
import { TxOutDataModel, TxInDataModel } from '.'
import { BufferTransformer } from '../../lib/BufferTransformer'

@Entity('tx')
export class TxDataModel {
  @PrimaryColumn('integer')
  id: number

  @Column({
    type: 'bytea',
    readonly: true,
    transformer: new BufferTransformer()
  })
  hash: string

  @Column('integer')
  block: number

  @Column('integer')
  fee: number

  @OneToMany(_type => TxInDataModel, txIn => txIn.transaction)
  inputs: TxInDataModel[]

  @OneToMany(_type => TxOutDataModel, txOut => txOut.transaction)
  outputs: TxOutDataModel[]
}
