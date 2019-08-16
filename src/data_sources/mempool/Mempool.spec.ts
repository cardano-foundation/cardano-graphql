import { expect } from 'chai'
import * as DataLoader from 'dataloader'
import { Mempool } from './Mempool'
import { transactions } from '../../lib/mocks'

describe('Mempool', () => {
  let mempool: Mempool
  beforeEach(() => {
    mempool = new Mempool({
      transactions
    })
    mempool.initialize()
  })
  describe('transaction', () => {
    it('is a DataLoader', () => {
      expect(mempool.transaction).to.be.an.instanceof(DataLoader)
    })
  })
  describe('transactionCount', () => {
    it('Returns a promise', async () => {
      const result = await mempool.transactionCount()
      expect(result).to.eq(2)
    })
  })
})
