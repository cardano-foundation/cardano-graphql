import { expect } from 'chai'
import { Ledger } from './Ledger'
import { blocks, transactions } from '../../lib/mocks'

const tx2 = transactions[0]
const tx3 = transactions[1]
const block1 = blocks[0]
const block2 = blocks[1]

describe('Ledger', () => {
  let ledger: Ledger
  beforeEach(() => {
    ledger = new Ledger({
      blocks,
      transactions
    })
    ledger.initialize()
  })
  describe('Loading transactions', () => {
    it('Returns a promise for a transaction by id', async () => {
      const result = await ledger.transaction.load('tx2')
      expect(result).to.deep.eq(tx2)
    })
    it('Returns a promise that resolves to null if not found', async () => {
      const result = await ledger.transaction.load('tx?')
      expect(result).to.eq(null)
    })
    it('Batches loads made within the same event tick, returning an array of promises', async () => {
      const result = await Promise.all([
        ledger.transaction.load('tx2'),
        ledger.transaction.load('tx?'),
        ledger.transaction.load('tx3')
      ])
      expect(result).to.deep.eq([tx2, null, tx3])
    })
  })
  describe('Loading blocks', () => {
    it('Returns a promise for a block by id', async () => {
      const result = await ledger.block.load('block1')
      expect(result).to.deep.eq(block1)
    })
    it('Returns a promise that resolves to null if not found', async () => {
      const result = await ledger.block.load('block?')
      expect(result).to.eq(null)
    })
    it('Batches loads made within the same event tick, returning an array of promises', async () => {
      const result = await Promise.all([
        ledger.block.load('block1'),
        ledger.block.load('block??'),
        ledger.block.load('block2')
      ])
      expect(result).to.deep.eq([block1, null, block2])
    })
  })
  describe('blockHeight', () => {
    it('Returns a promise', async () => {
      const result = await ledger.blockHeight()
      expect(result).to.eq(2)
    })
  })
})
