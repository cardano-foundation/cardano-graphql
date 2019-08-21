import * as fs from 'fs'
import * as path from 'path'
import { ApolloServer } from 'apollo-server'
import { getConfig } from './config'
import { Ledger, Mempool } from './data_sources'
import { transactions } from './lib/mocks'
import { Context } from './Context'
import { resolvers } from './resolvers'
import { Sequelize } from 'sequelize'

const config = getConfig()

const sequelize = new Sequelize('cexplorer', 'nix', 'password', {
  host: 'localhost',
  dialect: 'postgres'
})

const server = new ApolloServer({
  dataSources (): Context['dataSources'] {
    return {
      ledger: new Ledger(sequelize),
      mempool: new Mempool({ transactions })
    }
  },
  introspection: true,
  mocks: config.mockResponses,
  resolvers,
  tracing: config.tracing,
  typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
})

server.listen(config.apiPort).then(({ url }) => {
  console.log(`🚀  Server ready at ${url}`)
})
