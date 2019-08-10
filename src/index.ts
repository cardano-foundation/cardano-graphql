import { getConfig } from './config'
import { Server } from './Server'
import { InMemoryLedgerDataSource, InMemoryMempoolDataSource } from './data_sources'
import { transactions } from './lib/mocks'

const config = getConfig()
const server = Server({
  dataSources: {
    ledger: InMemoryLedgerDataSource({ transactions }),
    mempool: InMemoryMempoolDataSource({ transactions: [] })
  },
  mocks: config.mockResponses
})

server.listen(config.apiPort).then(({ url }) => {
  console.log(`🚀  Server ready at ${url}`)
})
