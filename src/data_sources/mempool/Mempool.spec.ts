import { expect } from 'chai'
import { Mempool } from './Mempool'
import { transactions } from '../../lib/mocks'

const tx2 = transactions[0]
const tx3 = transactions[1]

describe('Mempool', () => {
  let mempool: Mempool
  beforeEach(() => {
    mempool = new Mempool({
      transactions
    })
    mempool.initialize()
  })
  describe('Loading transactions', () => {
    it('Returns a promise for a transaction by id', async () => {
      const result = await mempool.transaction.load('tx2')
      expect(result).to.deep.eq(tx2)
    })
    it('Returns a promise that resolves to null if not found', async () => {
      const result = await mempool.transaction.load('tx?')
      expect(result).to.eq(null)
    })
    it('Batches loads made within the same event tick, returning an array of promises', async () => {
      const result = await Promise.all([
        mempool.transaction.load('tx2'),
        mempool.transaction.load('tx?'),
        mempool.transaction.load('tx3')
      ])
      expect(result).to.deep.eq([tx2, null, tx3])
    })
  })
  describe('transactionCount', () => {
    it('Returns a promise', async () => {
      const result = await mempool.transactionCount()
      expect(result).to.eq(2)
    })
  })
})
