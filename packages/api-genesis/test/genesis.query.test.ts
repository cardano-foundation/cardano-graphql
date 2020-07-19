import path from 'path'
import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

const shelleyTestnetGenesis = require('../../../config/network/shelley_testnet/genesis.json')
const mainnetCandidateGenesis = require('../../../config/network/mainnet_candidate/genesis.json')

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries'), name)
}

describe('genesis', () => {
  let shelleyTestnetClient: TestClient
  let mainnetCandidateClient: TestClient

  describe('Shelley testnet', () => {
    beforeAll(async () => {
      shelleyTestnetClient = await buildClient(shelleyTestnetGenesis)
      mainnetCandidateClient = await buildClient(mainnetCandidateGenesis)
    })

    it('Returns key information about the network', async () => {
      const query = { query: await loadQueryNode('keyNetworkInfo') }
      const shelleyTestnetResult = await shelleyTestnetClient.query(query)
      expect(shelleyTestnetResult.data).toMatchSnapshot()
      const mainnetCandidateResult = await mainnetCandidateClient.query(query)
      expect(mainnetCandidateResult.data).toMatchSnapshot()
    })

    it('Returns all information from genesis.json', async () => {
      const query = { query: await loadQueryNode('allInfo') }
      const shelleyTestnetResult = await shelleyTestnetClient.query(query)
      expect(shelleyTestnetResult.data).toMatchSnapshot()
      const mainnetCandidateResult = await shelleyTestnetClient.query(query)
      expect(mainnetCandidateResult.data).toMatchSnapshot()
    })
  })
})
