import { Block } from '../../graphql_types'
import { tx21c528, txa54489, txd9e280 } from './transaction_assertions'
import { DeepPartial } from '../DeepPartial'

export const block43177 = {
  epoch: {
    number: 2
  },
  fees: 171246,
  id: 'd2210effd1b09da47998c7fc9e890beb1825114e734c343dd920829e781c6325',
  merkelRootHash: '3d94dbe277e2a76fc0b1bbee29ff452658a4a01058c32eb2fd7b51d7e10fedab',
  number: 43177,
  previousBlock: {
    number: 43176
  },
  size: 2554,
  slot: {
    epoch: {
      number: 2
    },
    number: 1
  },
  transactions: [tx21c528, txa54489]
} as DeepPartial<Block>

export const block43178 = {
  epoch: {
    number: 2
  },
  fees: 171246,
  id: '7e89b8df0e4f7f16c04d6068d839052ff1a514054d7146625ce2c42511f80d6f',
  merkelRootHash: 'dc2718b3638853620c2f7e9f6fadbb43be3231c7406f5101173d75ef084cdc3f',
  number: 43178,
  previousBlock: {
    number: 43177
  },
  size: 4877,
  slot: {
    epoch: {
      number: 2
    },
    number: 2
  },
  transactions: [txd9e280]
} as DeepPartial<Block>
