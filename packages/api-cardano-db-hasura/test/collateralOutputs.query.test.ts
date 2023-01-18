import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'collateral_outputs'), name)
}

describe('collateralOutputs', () => {
  let client: TestClient
  beforeAll(async () => {
    ({ client } = await init('collateralOutputs'))
  })

  it('can return an array of collateral outputs', async () => {
    const result = await client.query({
      query: await loadQueryNode('collateralOutputs'),
      variables: {
        limit: 1
      }
    })
    expect(result.data.collateralOutputs[0].address).not.toBeNull()
    expect(result.data.collateralOutputs[0].value).not.toBeNull()
    expect(result.data.collateralOutputs[0].transaction).not.toBeNull()
  })

  it('can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('collateralOutputsAggregate')
    })
    expect(result.data.collateralOutputs_aggregate.aggregate.max.value).not.toBeNull()
    expect(result.data.collateralOutputs_aggregate.aggregate.min.value).not.toBeNull()
    expect(result.data.collateralOutputs_aggregate.aggregate.sum.value).not.toBeNull()
  })
})
