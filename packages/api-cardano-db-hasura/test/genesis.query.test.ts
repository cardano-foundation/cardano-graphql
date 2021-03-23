import path from 'path'
import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'genesis'), name)
}

describe('genesis', () => {
  let client: TestClient

  beforeAll(async () => {
    client = await testClient.mainnet()
  })

  it('Returns key information about the network genesis', async () => {
    const query = { query: await loadQueryNode('keyNetworkInfo') }
    const result = await client.query(query)
    expect(result.data).toMatchSnapshot()
  })
})
