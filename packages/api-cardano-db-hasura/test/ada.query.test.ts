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

const alonzoPurpleGenesis = {
  byron: require('../../../config/network/alonzo-purple/genesis/byron.json'),
  shelley: require('../../../config/network/alonzo-purple/genesis/shelley.json')
} as Genesis

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'ada'), name)
}

describe('ada', () => {
  let client: TestClient
  let alonzoPurpleClient: TestClient
  beforeAll(async () => {
    client = await testClient.mainnet()
    alonzoPurpleClient = await testClient.alonzoPurple()
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

  it('returns ada supply information - alonzo-purple', async () => {
    const result = await alonzoPurpleClient.query({
      query: await loadQueryNode('adaSupply')
    })
    const { ada } = result.data
    const circulatingSupply = new BigNumber(ada.supply.circulating).toNumber()
    const maxSupply = new BigNumber(ada.supply.max).toNumber()
    const totalSupply = new BigNumber(ada.supply.total).toNumber()
    expect(maxSupply).toEqual(alonzoPurpleGenesis.shelley.maxLovelaceSupply)
    expect(maxSupply).toBeGreaterThan(circulatingSupply)
    expect(totalSupply).toBeGreaterThan(circulatingSupply)
    expect(totalSupply).toBeLessThan(maxSupply)
  })
})
