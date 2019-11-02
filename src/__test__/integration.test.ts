import { run as blocksTests } from './blocks.query.test'
import { run as cardanoTests } from './cardano.query.test'
import { run as epochTests } from './epochs.query.test'
import { run as transactionTests } from './transactions.query.test'
import { run as utxoSetTests } from './utxoSet.query.test'

import { createTestClient } from 'apollo-server-testing'
import { TestApolloServer } from './TestApolloServer'

const tests = [
  blocksTests,
  cardanoTests,
  epochTests,
  transactionTests,
  utxoSetTests
]

const createClient = async () => createTestClient(await TestApolloServer())
tests.forEach(t => t(createClient))
