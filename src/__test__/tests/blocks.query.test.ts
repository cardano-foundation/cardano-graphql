import gql from 'graphql-tag'
import { block29021, block29022 } from '../data_assertions'
import { TestClient } from '../TestClient'
import { loadExampleQueryNode } from '../../util'

export function blocksTests (makeClient: () => Promise<TestClient>) {
  describe('blocks', () => {
    let client: TestClient
    beforeEach(async () => {
      client = await makeClient()
    }, 60000)

    it('caps the response to 100 blocks', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('blocks', 'blockHashesNoArgs')
      })
      expect(result.data.blocks.length).toBe(100)
    })

    it('allows custom pagination size with a limit and offset', async () => {
      const page1 = await client.query({
        query: await loadExampleQueryNode('blocks', 'first20Blocks')
      })
      const page2 = await client.query({
        query: await loadExampleQueryNode('blocks', 'second20Blocks')
      })
      expect(page1.data.blocks.length).toBe(20)
      expect(page1.data.blocks[19].number).toBe(23)
      expect(page2.data.blocks.length).toBe(20)
      expect(page2.data.blocks[19].number).toBe(43)
    })

    it('Can return blocks by number', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('blocks', 'blockByNumber'),
        variables: { number: 29022 }
      })
      expect(result.data.blocks.length).toBe(1)
      expect(result.data.blocks[0]).toEqual({ hash: block29022.basic.hash })
      expect(result.data).toMatchSnapshot()
    })

    it('Can return blocks by an array of hashes', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('blocks', 'blocksByHashes'),
        variables: { hashes: [block29021.basic.hash, block29022.basic.hash] }
      })
      expect(result.data.blocks.length).toBe(2)
      expect(result.data.blocks).toEqual(
        expect.arrayContaining([
          expect.objectContaining(block29021.basic),
          expect.objectContaining(block29022.basic)
        ])
      )
    })

    it('Can return aggregated data', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('blocks', 'aggregateDataWithinBlock'),
        variables: { number: 29021, epochLessThan: 185 }
      })
      expect(result.data.blocks[0]).toEqual(block29021.aggregated)
      expect(result.data).toMatchSnapshot()
    })

    it('Can return filtered aggregated data', async () => {
      const result = await client.query({
        query: gql`query {
            blocks( where: { number: { _eq: 29021 }}) {
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
      expect(result.data.blocks[0]).toEqual(block29021.aggregated_filtered)
      expect(result.data).toMatchSnapshot()
    })

    it('are linked to their predecessor, and the chain can be traversed', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('blocks', 'selectGreatGrandparentBlock'),
        variables: { number: 29022 }
      })
      expect(result.data.blocks[0].previousBlock.previousBlock.previousBlock.number).toBe(29019)
      expect(result.data).toMatchSnapshot()
    })

    it('are linked to their successor, and the chain can be traversed', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('blocks', 'selectGreatGrandchildBlock'),
        variables: { number: 29022 }
      })
      expect(result.data.blocks[0].nextBlock.nextBlock.nextBlock.number).toBe(29025)
      expect(result.data).toMatchSnapshot()
    })
  })
}
