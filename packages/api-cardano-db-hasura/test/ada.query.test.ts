import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'
import { Genesis } from '@src/graphql_types'
import BigNumber from 'bignumber.js'

const mainnetGenesis = {
  byron: require('../../../config/network/mainnet/genesis/byron.json'),
  shelley: require('../../../config/network/mainnet/genesis/shelley.json')
} as Genesis

const alonzoQaGenesis = {
  byron: require('../../../config/network/mainnet/genesis/byron.json'),
  shelley: require('../../../config/network/mainnet/genesis/shelley.json')
} as Genesis

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'ada'), name)
}

describe('ada', () => {
  let client: TestClient
  let alonzoQaClient: TestClient
  beforeAll(async () => {
    client = await testClient.mainnet()
    alonzoQaClient = await testClient.alonzoQa()
  })

  it('returns ada supply information - mainnet', async () => {
    const result = await client.query({
      query: await loadQueryNode('adaSupply')
    })
    const { ada } = result.data
    const circulatingSupply = new BigNumber(ada.supply.circulating).toNumber()
    const maxSupply = new BigNumber(ada.supply.max).toNumber()
    const totalSupply = new BigNumber(ada.supply.total).toNumber()
    expect(maxSupply).toEqual(mainnetGenesis.shelley.maxLovelaceSupply)
    expect(maxSupply).toBeGreaterThan(circulatingSupply)
    expect(totalSupply).toBeGreaterThan(circulatingSupply)
    expect(totalSupply).toBeLessThan(maxSupply)
  })

  it('returns ada supply information - alonzo-qa', async () => {
    const result = await alonzoQaClient.query({
      query: await loadQueryNode('adaSupply')
    })
    const { ada } = result.data
    const circulatingSupply = new BigNumber(ada.supply.circulating).toNumber()
    const maxSupply = new BigNumber(ada.supply.max).toNumber()
    const totalSupply = new BigNumber(ada.supply.total).toNumber()
    expect(maxSupply).toEqual(alonzoQaGenesis.shelley.maxLovelaceSupply)
    expect(maxSupply).toBeGreaterThan(circulatingSupply)
    expect(totalSupply).toBeGreaterThan(circulatingSupply)
    expect(totalSupply).toBeLessThan(maxSupply)
  })
})
