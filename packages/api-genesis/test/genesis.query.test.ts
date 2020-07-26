import path from 'path'
import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

const genesisFiles = {
  mainnet: {
    byron: require('../../../config/network/mainnet/genesis_byron.json'),
    shelley: require('../../../config/network/mainnet/genesis_shelley.json')
  },
  shelley_testnet: {
    shelley: require('../../../config/network/shelley_testnet/genesis_shelley.json')
  }
}

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries'), name)
}

describe('genesis', () => {
  let client: TestClient

  describe('Single-era', () => {
    beforeAll(async () => {
      client = await buildClient(genesisFiles.shelley_testnet)
    })

    it('Returns key information about the network', async () => {
      const query = { query: await loadQueryNode('keyNetworkInfoShelley') }
      const result = await client.query(query)
      expect(result.data).toMatchSnapshot()
    })

    it('Returns all information from genesis.json', async () => {
      const query = { query: await loadQueryNode('allInfoShelley') }
      const result = await client.query(query)
      expect(result.data).toMatchSnapshot()
    })
  })

  describe('Spanning-eras', () => {
    beforeAll(async () => {
      client = await buildClient(genesisFiles.mainnet)
    })

    it('Returns key information about the network', async () => {
      const query = { query: await loadQueryNode('keyNetworkInfoByronShelley') }
      const result = await client.query(query)
      expect(result.data).toMatchSnapshot()
    })
  })
})
