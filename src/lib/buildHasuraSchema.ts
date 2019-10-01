import { RetryPromise } from 'promise-exponential-retry'
import { createHttpLink } from 'apollo-link-http'
import { fetch } from 'cross-fetch'
import { introspectSchema, makeRemoteExecutableSchema } from 'graphql-tools'

export async function buildHasuraSchema (hasuraUri: string) {
  const link = createHttpLink({
    uri: hasuraUri,
    fetch
  })

  await RetryPromise.retryPromise('Hasura Schema introspection', async () => {
    const schema = await introspectSchema(link)
    const baseBlockType = schema.getType('Block')
    if (!baseBlockType) {
      throw new Error('Remote schema is missing')
    }
  }, 30)

  return makeRemoteExecutableSchema({
    schema: await introspectSchema(link),
    link
  })
}
