import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'scripts'), name)
}

describe('scripts', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.alonzoPurple()
  })

  it('can return an array of timelock scripts', async () => {
    const result = await client.query({
      query: await loadQueryNode('timelockScripts'),
      variables: {
        limit: 1
      }
    })
    expect(result.data.scripts[0].hash).not.toBeNull()
    expect(result.data.scripts[0].serialisedSize).toBeNull()
    expect(result.data.scripts[0].transaction).not.toBeNull()
    expect(result.data.scripts[0].type).not.toBeNull()
  })

  it('can return an array of plutus scripts', async () => {
    const result = await client.query({
      query: await loadQueryNode('plutusScripts'),
      variables: {
        limit: 1
      }
    })
    expect(result.data.scripts[0].hash).not.toBeNull()
    expect(result.data.scripts[0].serialisedSize).not.toBeNull()
    expect(result.data.scripts[0].transaction).not.toBeNull()
    expect(result.data.scripts[0].type).not.toBeNull()
  })

  it('can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('scriptsAggregate')
    })
    expect(result.data.scripts_aggregate.aggregate.avg.serialisedSize).not.toBeNull()
    expect(result.data.scripts_aggregate.aggregate.max.serialisedSize).not.toBeNull()
    expect(result.data.scripts_aggregate.aggregate.min.serialisedSize).not.toBeNull()
    expect(result.data.scripts_aggregate.aggregate.sum.serialisedSize).not.toBeNull()
  })
})
