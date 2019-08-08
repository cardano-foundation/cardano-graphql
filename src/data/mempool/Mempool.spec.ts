import { expect } from 'chai'
import { InMemoryMempool, MempoolProvider } from './'
import { testTransactions } from '../'
const tx2 = testTransactions[0]
const tx3 = testTransactions[1]

describe('MempoolProvider', () => {
  let mempool: MempoolProvider
  describe('findById', () => {
    beforeEach(() => { mempool = InMemoryMempool(testTransactions) })

    it('Can find a transaction ID', async () => {
      expect(await mempool.transactionById('tx2')).to.eq(tx2)
    })
    it('Is null if not found', async () => {
      expect(await mempool.transactionById('tx?')).to.be.null
    })
  })
  describe('findByIds', () => {
    beforeEach(() => { mempool = InMemoryMempool(testTransactions) })

    it('Can find a batch of transactions by ID', async () => {
      expect(await mempool.transactionsByIds(['tx2', 'tx3'])).to.deep.eq([tx2, tx3])
    })
    it('Returns partially matching collection', async () => {
      expect(await mempool.transactionsByIds(['tx2', 'tx?'])).to.deep.eq([tx2])
    })
    it('Is null if none are found', async () => {
      expect(await mempool.transactionsByIds(['tx?'])).to.be.null
    })
  })
  describe('has', () => {
    beforeEach(() => { mempool = InMemoryMempool(testTransactions) })

    it('can provide a simple boolean response', async () => {
      expect(await mempool.has('tx2')).to.be.true
      expect(await mempool.has('tx?')).to.be.false
    })
  })
  describe('transactionCount', () => {
    it('counts the number of transactions', async () => {
      mempool = InMemoryMempool(testTransactions)
      expect(await mempool.transactionCount()).to.eq(2)
      mempool = InMemoryMempool([])
      expect(await mempool.transactionCount()).to.eq(0)
    })
  })
})
