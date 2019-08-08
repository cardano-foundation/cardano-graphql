import { expect } from 'chai'
import { InMemoryTransactions, testTransactions } from './'
const tx2 = testTransactions[0]
const tx3 = testTransactions[1]

describe('InMemoryTransactions', () => {
  let repository: ReturnType<typeof InMemoryTransactions>
  beforeEach(() => {
    repository = InMemoryTransactions(testTransactions)
  })
  describe('findById', () => {
    it('Can find a transaction ID', async () => {
      expect(await repository.findById('tx2')).to.eq(tx2)
    })
    it('Is null if not found', async () => {
      expect(await repository.findById('tx?')).to.be.null
    })
  })
  describe('findByIds', () => {
    it('Can find a batch of transactions by ID', async () => {
      expect(await repository.findByIds(['tx2', 'tx3'])).to.deep.eq([tx2, tx3])
    })
    it('Returns partially matching collection', async () => {
      expect(await repository.findByIds(['tx2', 'tx?'])).to.deep.eq([tx2])
    })
    it('Is null if none are found', async () => {
      expect(await repository.findByIds(['tx?'])).to.be.null
    })
  })
  describe('has', () => {
    it('can provide a simple boolean response', async () => {
      expect(await repository.has('tx2')).to.be.true
      expect(await repository.has('tx?')).to.be.false
    })
  })
})
