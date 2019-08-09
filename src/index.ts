import { getConfig } from './config'
import { Server } from './Server'
import { InMemoryMempool as Mempool, InMemoryLedger as Ledger } from './data'
import { transactions } from './data/mocks'

const config = getConfig()
const server = Server({
  dataSources: {
    ledger: Ledger({ transactions }),
    mempool: Mempool({ transactions: [] })
  },
  mocks: config.mockResponses
})

server.listen(config.apiPort).then(({ url }) => {
  console.log(`🚀  Server ready at ${url}`)
})
