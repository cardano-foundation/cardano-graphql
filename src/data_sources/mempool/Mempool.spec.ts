import { expect } from 'chai'
import { InMemoryMempoolDataSource as Mempool, MempoolDataSource } from './'
import { transactions } from '../../lib/mocks'
const tx2 = transactions[0]
const tx3 = transactions[1]

describe('MempoolDataSource', () => {
  let mempool: MempoolDataSource
  describe('Getting a transaction', () => {
    beforeEach(() => { mempool = Mempool({ transactions }) })

    it('Can get a transaction by ID', async () => {
      expect(await mempool.getTransaction('tx2')).to.eq(tx2)
    })
    it('Is null if not found', async () => {
      expect(await mempool.getTransaction('tx?')).to.be.null
    })
  })
  beforeEach(() => { mempool = Mempool({ transactions }) })
  describe('Getting a batch of transactions', () => {
    it('Gets all if no filter passed', async () => {
      expect(await mempool.getTransactions()).to.deep.eq(transactions)
    })
    it('Can get a batch of transactions by ID', async () => {
      expect(await mempool.getTransactions(['tx2', 'tx3'])).to.deep.eq([tx2, tx3])
    })
    it('Returns partially matching collection', async () => {
      expect(await mempool.getTransactions(['tx2', 'tx?'])).to.deep.eq([tx2])
    })
    it('Is null if none are found', async () => {
      expect(await mempool.getTransactions(['tx?'])).to.be.null
    })
  })
  describe('has', () => {
    beforeEach(() => { mempool = Mempool({ transactions }) })

    it('can provide a simple boolean response', async () => {
      expect(await mempool.has('tx2')).to.be.true
      expect(await mempool.has('tx?')).to.be.false
    })
  })
  describe('transactionCount', () => {
    it('counts the number of transactions', async () => {
      mempool = Mempool({ transactions })
      expect(await mempool.transactionCount()).to.eq(2)
      mempool = Mempool({ transactions: [] })
      expect(await mempool.transactionCount()).to.eq(0)
    })
  })
})
