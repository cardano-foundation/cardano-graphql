import { Config } from './config'
import { Server } from './Server'
import {
  buildSchema as buildCardanoDbHasuraSchema,
  Db,
  Genesis,
  HasuraClient
} from '@cardano-graphql/api-cardano-db-hasura'
import { GraphQLSchema } from 'graphql'

export * from './config'

export async function CompleteApiServer (config: Config): Promise<Server> {
  const schemas: GraphQLSchema[] = []
  let genesis: Genesis
  if (config.genesis.byronPath !== undefined || config.genesis.shelleyPath !== undefined) {
    genesis = {
      ...config.genesis.byronPath !== undefined ? { byron: require(config.genesis.byronPath) } : {},
      ...config.genesis.shelleyPath !== undefined ? { shelley: require(config.genesis.shelleyPath) } : {}
    }
  }
  if (config.hasuraUri !== undefined) {
    const hasuraClient = new HasuraClient(config.hasuraCliPath, config.hasuraUri)
    const db = new Db(config.db)
    await db.init({
      onDbSetup: hasuraClient.applySchemaAndMetadata.bind(hasuraClient)
    })
    schemas.push(await buildCardanoDbHasuraSchema(hasuraClient, genesis))
  }
  return new Server(schemas, config)
}
