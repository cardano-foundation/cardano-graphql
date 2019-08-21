import { expect } from 'chai'
import * as DataLoader from 'dataloader'
import { Ledger } from './Ledger'
import { transactions } from '../../lib/mocks'
import { Sequelize } from 'sequelize'

const tx2 = transactions[0]
const tx3 = transactions[1]

const sequelize = new Sequelize('cexplorer', 'nix', 'password', {
  host: 'localhost',
  dialect: 'postgres'
})

describe('Ledger', () => {
  let ledger: Ledger
  beforeEach(() => {
    ledger = new Ledger(sequelize)
    ledger.initialize()
  })
  describe('transaction', () => {
    it('is a DataLoader', () => {
      expect(ledger.transaction).to.be.an.instanceof(DataLoader)
    })
    it('Promises a transaction by id', async () => {
      const result = await ledger.transaction.load('tx2')
      expect(result).to.deep.eq(tx2)
    })
    it('Resolves to null if not found', async () => {
      const result = await ledger.transaction.load('tx?')
      expect(result).to.eq(null)
    })
    it('Performs a batch load for individual calls made within the same event tick, promising an array of transactions in the same order as requested', async () => {
      const result = await Promise.all([
        ledger.transaction.load('tx2'),
        ledger.transaction.load('tx?'),
        ledger.transaction.load('tx3')
      ])
      const result2 = await Promise.all([
        ledger.transaction.load('tx3'),
        ledger.transaction.load('tx2')
      ])
      const result3 = await Promise.all([
        ledger.transaction.load('tx2'),
        ledger.transaction.load('tx2')
      ])
      expect(result).to.deep.eq([tx2, null, tx3])
      expect(result2).to.deep.eq([tx3, tx2])
      expect(result3).to.deep.eq([tx2, tx2])
    })
    it('Provides a convenience method to load many in one call', async () => {
      const result = await ledger.transaction.loadMany(['tx2', 'tx3'])
      const result2 = await Promise.all([
        ledger.transaction.load('tx2'),
        ledger.transaction.load('tx3')
      ])
      expect(result).to.deep.eq(result2)
    })
  })
  describe('block', () => {
    it('is a DataLoader', () => {
      expect(ledger.transaction).to.be.an.instanceof(DataLoader)
    })
  })
  describe('blockHeight', () => {
    it('Returns a promise', async () => {
      const result = await ledger.blockHeight()
      expect(result).to.eq(2)
    })
  })
})
