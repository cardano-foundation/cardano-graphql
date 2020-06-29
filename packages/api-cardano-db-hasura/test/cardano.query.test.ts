import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import utilDev, { TestClient } from '@cardano-graphql/util-dev'
import { buildSchema } from '../src'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'cardano'), name)
}

describe('cardano', () => {
  let client: TestClient
  beforeAll(async () => {
    if (process.env.TEST_MODE === 'e2e') {
      client = await utilDev.createE2EClient()
    } else {
      const schema = await buildSchema('http://localhost:8090')
      client = await utilDev.createIntegrationClient(schema)
    }
  }, 60000)

  it('Returns static information about the network', async () => {
    const result = await client.query({
      query: await loadQueryNode('static')
    })
    expect(result.data).toMatchSnapshot()
  })
  it('Returns dynamic information about the network', async () => {
    const result = await client.query({
      query: await loadQueryNode('dynamic')
    })
    expect(result.data.cardano.blockHeight).toBeGreaterThan(3994551)
    expect(result.data.cardano.currentEpoch.number).toBeGreaterThan(184)
  })
})
