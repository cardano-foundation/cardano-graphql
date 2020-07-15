import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, 'queries'), name)
}

describe('Shelley era queries', () => {
  let mainnetClient: TestClient
  let mc4Client: TestClient
  beforeAll(async () => {
    mainnetClient = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442)
    mc4Client = await buildClient('http://localhost:3102', 'http://localhost:8092', 5444)
  }, 15000)

  it('will not throw errors during the Byron era', async () => {
    const result = await mainnetClient.query({
      query: await loadQueryNode('shelleyEraQueries')
    })
    console.log(result)
    // expect(result.data).toMatchSnapshot()
  })
  it('Shelley era smoke test', async () => {
    const result = await mc4Client.query({
      query: await loadQueryNode('shelleyEraQueries')
    })
    console.log(result)
    // expect(result.data).toMatchSnapshot()
  })
})
