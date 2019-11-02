import { ApolloClient, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import { RetryPromise } from 'promise-exponential-retry'
import fetch from 'cross-fetch'
import { run as blocksTests } from './blocks.query.test'
import { run as cardanoTests } from './cardano.query.test'
import { run as epochTests } from './epochs.query.test'
import { run as transactionTests } from './transactions.query.test'
import { run as utxoSetTests } from './utxoSet.query.test'

const tests = [
  blocksTests,
  cardanoTests,
  epochTests,
  transactionTests,
  utxoSetTests
]

const createClient = async () => {
  const client = new ApolloClient({
    cache: new InMemoryCache({
      addTypename: false
    }),
    link: createHttpLink({
      uri: 'http://localhost:3100',
      fetch
    })
  })
  await RetryPromise.retryPromise(
    'Checking GraphQL server is ready',
    async () => {
      await client.query({
        query: gql`query { cardano { blockHeight }}`
      })
    }, 50)
  return client
}

tests.forEach(t => t(createClient))
