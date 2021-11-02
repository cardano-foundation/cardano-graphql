import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'collateral_inputs'), name)
}

describe('collateralInputs', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.testnet()
  })

  it('can return an array of collateral inputs', async () => {
    const result = await client.query({
      query: await loadQueryNode('collateralInputs'),
      variables: {
        limit: 1
      }
    })
    expect(result.data.collateralInputs[0].address).not.toBeNull()
    expect(result.data.collateralInputs[0].sourceTransaction).not.toBeNull()
    expect(result.data.collateralInputs[0].value).not.toBeNull()
    expect(result.data.collateralInputs[0].transaction).not.toBeNull()
  })

  it('can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('collateralInputsAggregate')
    })
    expect(result.data.collateralInputs_aggregate.aggregate.avg.value).not.toBeNull()
    expect(result.data.collateralInputs_aggregate.aggregate.max.value).not.toBeNull()
    expect(result.data.collateralInputs_aggregate.aggregate.min.value).not.toBeNull()
    expect(result.data.collateralInputs_aggregate.aggregate.sum.value).not.toBeNull()
  })
})
