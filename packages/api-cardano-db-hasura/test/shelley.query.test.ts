import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, 'queries'), name)
}

describe('Shelley era queries', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442)
  }, 15000)

  it('will not throw errors during the Byron era', async () => {
    await client.query({
      query: await loadQueryNode('shelleyEraQueries')
    })
    // console.log(result)
    // expect(result.data).toMatchSnapshot()
  })
  it('Shelley era smoke test', async () => {
    await client.query({
      query: await loadQueryNode('shelleyEraQueries')
    })
    // console.log(result)
    // expect(result.data).toMatchSnapshot()
  })
})
