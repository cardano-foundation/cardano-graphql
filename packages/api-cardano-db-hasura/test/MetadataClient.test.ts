/**
 * @jest-environment node
 */
import { MetadataClient } from '@src/MetadataClient'

describe('MetadataClient', () => {
  let client: MetadataClient

  describe('initialize', () => {
    it('succeeds if the metadata server is reachable', async () => {
      client = new MetadataClient(true, 'https://tokens.cardano.org')
      await client.initialize()
      expect(client.state).toBe('initialized')
    })
  })

  describe('fetch', () => {
    it('throws if not initialized', async () => {
      expect.assertions(1)
      client = new MetadataClient(true, 'https://tokens.cardano.org')
      try {
        await client.fetch(['f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc53541474958'])
      } catch (error) {
        await expect(error.name).toBe('ModuleIsNotInitialized')
      }
    })
    it('can return all metadata by assetId', async () => {
      client = new MetadataClient(true, 'https://tokens.cardano.org/')
      await client.initialize()
      const response = await client.fetch(['f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc53541474958'])
      expect(response).toBeDefined()
      expect(response[0].metadata.decimals.value).toBe(8)
      expect(response[0].metadata.name.value).toBe('SingularityNet AGIX Token')
      expect(response[0].metadata.ticker.value).toBe('AGIX')
      expect(response[0].metadata.url.value).toBe('https://singularitynet.io/')
    })
  })
})
