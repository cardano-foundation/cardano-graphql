import { getConfig } from './config'
import { Server } from './Server'
import { InMemoryMempool, InMemoryTransactions, testTransactions } from './data'

const config = getConfig()
const server = Server({
  dataSources: {
    mempool: InMemoryMempool([]),
    transactions: InMemoryTransactions(testTransactions)
  },
  mocks: config.mockResponses
})

server.listen(config.apiPort).then(({ url }) => {
  console.log(`🚀  Server ready at ${url}`)
})
