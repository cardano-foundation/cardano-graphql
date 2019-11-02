import { createTestClient } from 'apollo-server-testing'
import * as tests from './tests'
import { TestApolloServer } from './TestApolloServer'

const createClient = async () => createTestClient(await TestApolloServer())
Object.values(tests).forEach(test => test(createClient))
