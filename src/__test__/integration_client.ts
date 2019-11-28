import { createTestClient } from 'apollo-server-testing'
import { TestApolloServer } from './TestApolloServer'

export const createClient = async () => createTestClient(await TestApolloServer())
