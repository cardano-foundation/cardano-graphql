import { TestClient } from '../TestClient'
import { loadExampleQueryNode } from '../../util'

export function cardanoTests (createClient: () => Promise<TestClient>) {
  describe('cardano', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

    it('Returns static information about the network', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('cardano', 'static')
      })
      expect(result.data).toMatchSnapshot()
    })
    it('Returns dynamic information about the network', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('cardano', 'dynamic')
      })
      expect(result.data.cardano.blockHeight).toBeGreaterThan(3994551)
      expect(result.data.cardano.currentEpoch.number).toBeGreaterThan(184)
    })
  })
}
