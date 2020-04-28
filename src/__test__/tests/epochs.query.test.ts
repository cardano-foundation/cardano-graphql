import gql from 'graphql-tag'
import { epoch1 } from '../data_assertions'
import { TestClient } from '../TestClient'
import { loadQueryNode } from '../../util'

export function epochTests (createClient: () => Promise<TestClient>) {
  describe('epochs', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

    it('Returns epoch details by number', async () => {
      const result = await client.query({
        query: await loadQueryNode('epochs', 'epochDetailsByNumber'),
        variables: { number: 1 }
      })
      expect(result.data.epochs[0]).toEqual(epoch1.basic)
      expect(result.data).toMatchSnapshot()
    })

    it('Can return aggregated data', async () => {
      const result = await client.query({
        query: await loadQueryNode('epochs', 'aggregateDataWithinEpoch'),
        variables: { number: 1 }
      })
      expect(result.data.epochs[0]).toEqual(epoch1.aggregated)
      expect(result.data.epochs[0].blocksCount).toEqual(epoch1.aggregated.blocks_aggregate.aggregate.count)
      expect(result.data).toMatchSnapshot()
    })

    it('Can return filtered aggregated data', async () => {
      const result = await client.query({
        query: await loadQueryNode('epochs', 'numberOfBlocksProducedByLeaderInEpoch'),
        variables: { number: 1, slotLeader: 'SlotLeader-6c9e14978b9d6629' }
      })
      expect(result.data).toMatchSnapshot()
    })

    it('Returns epoch details by number range', async () => {
      // Todo: Convert this into an actual ranged query now the performance issue is resolved.
      const result = await client.query({
        query: await loadQueryNode('epochs', 'epochDetailsInRange'),
        variables: { numbers: [1] }
      })
      expect(result.data.epochs[0]).toEqual(epoch1.basic)
      expect(result.data).toMatchSnapshot()
    })

    it('Can return aggregated Epoch data', async () => {
      const result = await client.query({
        query: await loadQueryNode('epochs', 'aggregateEpochData'),
        variables: { epochNumberLessThan: 185 }
      })
      expect(result.data).toMatchSnapshot()
    })

    it('Returns blocks scoped to epoch', async () => {
      const validQueryResult = await client.query({
        query: await loadQueryNode('epochs', 'blocksInEpoch'),
        variables: { number: 1, blockLimit: 1 }
      })
      const invalidQueryResult = await client.query({
        query: gql`query {
            epochs( where: { number: { _eq: 1 }}) {
                blocks(limit: 20, where: { epoch: { number: { _eq: 0 } }}) {
                    id
                }
            }
        }`
      })
      expect(validQueryResult.data.epochs[0].blocks[0].epoch.number).toBe(1)
      expect(invalidQueryResult.data.epochs[0].blocks.length).toBe(0)
    })
  })
}
