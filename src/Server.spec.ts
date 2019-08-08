import { expect, use } from 'chai'
import * as chaiAsPromised from 'chai-as-promised'
import { Server } from './Server'
import { Client } from './Client'
import { InMemoryTransactions, testTransactions } from './data'

use(chaiAsPromised)

describe('Server', () => {
  let server: ReturnType<typeof Server>
  let client: ReturnType<typeof Client>
  const API_PORT = 4100
  const apiUri = `http://localhost:${API_PORT}`

  beforeEach(() => {
    client = Client({ apiUri })
  })

  describe('Schema introspection', () => {
    it('Allows introspection of the schema by default, including in production', async () => {
      process.env.NODE_ENV = 'production'
      server = Server({
        dataSources: { transactions: InMemoryTransactions(testTransactions) }
      })
      await server.listen(API_PORT)
      expect((await client.schema()).data.__schema).to.exist
      await server.stop()
    })
    it('Can be disabled', async () => {
      server = Server({
        dataSources: { transactions: InMemoryTransactions(testTransactions) },
        introspection: false
      })
      await server.listen(API_PORT)
      expect(client.schema()).to.eventually.be.rejected
      await server.stop()
    })
  })
})
