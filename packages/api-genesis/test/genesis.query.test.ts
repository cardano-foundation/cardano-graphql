import path from 'path'
import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

const genesisFiles = {
  mainnet: {
    byron: require('../../../config/network/mainnet/genesis/byron.json'),
    shelley: require('../../../config/network/mainnet/genesis/shelley.json')
  }
}

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries'), name)
}

describe('genesis', () => {
  let client: TestClient

  beforeAll(async () => {
    client = await buildClient(genesisFiles.mainnet)
  })

  it('Returns key information about the network genesis', async () => {
    const query = { query: await loadQueryNode('keyNetworkInfo') }
    const result = await client.query(query)
    expect(result.data).toMatchSnapshot()
  })
})
