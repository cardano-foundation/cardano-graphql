import { expect } from 'chai'
import { InMemoryLedger as Ledger, LedgerProvider } from './'
import { transactions } from '../mocks'
const tx2 = transactions[0]
const tx3 = transactions[1]

describe('LedgerProvider', () => {
  let ledger: LedgerProvider
  beforeEach(() => {
    ledger = Ledger({ transactions })
  })
  describe('finding a single transaction', () => {
    it('Can find a transaction ID', async () => {
      expect(await ledger.transaction('tx2')).to.eq(tx2)
    })
    it('Is null if not found', async () => {
      expect(await ledger.transaction('tx?')).to.be.null
    })
  })
  describe('findByIds', () => {
    it('Can find a batch of transactions by ID', async () => {
      expect(await ledger.transactions(['tx2', 'tx3'])).to.deep.eq([tx2, tx3])
    })
    it('Returns partially matching collection', async () => {
      expect(await ledger.transactions(['tx2', 'tx?'])).to.deep.eq([tx2])
    })
    it('Is null if none are found', async () => {
      expect(await ledger.transactions(['tx?'])).to.be.null
    })
  })
  describe('has', () => {
    it('can provide a simple boolean response', async () => {
      expect(await ledger.has('tx2')).to.be.true
      expect(await ledger.has('tx?')).to.be.false
    })
  })
})
