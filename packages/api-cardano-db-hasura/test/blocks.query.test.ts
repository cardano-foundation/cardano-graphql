import path from 'path'
import { DocumentNode } from 'graphql'
import gql from 'graphql-tag'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'blocks'), name)
}

const allFieldsPopulated = (obj: any) => {
  let k: keyof typeof obj
  for (k in obj) {
    if (
      typeof k === 'object'
    ) {
      allFieldsPopulated(k)
    }
    expect(k).not.toBeNull()
  }
}

describe('blocks', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('caps the response to 100 blocks', async () => {
    const result = await client.query({
      query: await loadQueryNode('blockHashesNoArgs')
    })
    expect(result.data.blocks.length).toBe(2500)
  })

  it('allows custom pagination size with a limit and offset', async () => {
    const page1 = await client.query({
      query: await loadQueryNode('first20Blocks')
    })
    const page2 = await client.query({
      query: await loadQueryNode('second20Blocks')
    })
    expect(page1.data.blocks.length).toBe(20)
    expect(page1.data.blocks[19].number).toBe(23)
    expect(page2.data.blocks.length).toBe(20)
    expect(page2.data.blocks[19].number).toBe(43)
  })

  it('Can return blocks by number', async () => {
    const result = await client.query({
      query: await loadQueryNode('blockByNumbers'),
      variables: { numbers: [100, 200] }
    })
    expect(result.data.blocks.length).toBe(2)
    expect(result.data.blocks[0].hash).not.toBeNull()
    expect(result.data.blocks[1].hash).not.toBeNull()
  })

  // Todo: Restore. Assertion is valid, but matcher seems to have 'ArrayContaining [ObjectContaining' appended
  // it('Can return blocks by an array of hashes', async () => {
  //   const result = await client.query({
  //     query: await loadQueryNode('blocksByHashes'),
  //     variables: { hashes: [block29021.basic.hash, block29022.basic.hash] }
  //   })
  //   expect(result.data.blocks.length).toBe(2)
  //   console.log()
  //   expect(result.data.blocks).toEqual(
  //     expect.arrayContaining([
  //       expect.objectContaining(block29021.basic),
  //       expect.objectContaining(block29022.basic)
  //     ])
  //   )
  // })

  it('Can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinBlock'),
      variables: { number: 283413, epochLessThan: 50 }
    })
    allFieldsPopulated(result.data.blocks[0])
  })

  it('Can return filtered aggregated data', async () => {
    const result = await client.query({
      query: gql`query {
          blocks( where: { number: { _eq: 283413 }}) {
              transactions_aggregate(
                  where: {
                      _and: [
                          { fee: { _gt: "10" }},
                          { totalOutput: { _lt: "4924799478670" } }
                      ]
                  }) {
                  aggregate {
                      count
                  }
              }
              number
          }
      }`
    })
    expect(result.data).toMatchSnapshot()
  })

  it('are linked to their predecessor, and the chain can be traversed', async () => {
    const result = await client.query({
      query: await loadQueryNode('selectGreatGrandparentBlock'),
      variables: { number: 29022 }
    })
    expect(result.data.blocks[0].previousBlock.previousBlock.previousBlock.number).toBe(29019)
    expect(result.data).toMatchSnapshot()
  })

  it('are linked to their successor, and the chain can be traversed', async () => {
    const result = await client.query({
      query: await loadQueryNode('selectGreatGrandchildBlock'),
      variables: { number: 29022 }
    })
    expect(result.data.blocks[0].nextBlock.nextBlock.nextBlock.number).toBe(29025)
    expect(result.data).toMatchSnapshot()
  })
})
