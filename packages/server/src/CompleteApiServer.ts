import { Config } from './config'
import { Server } from './Server'
import { buildSchema as buildCardanoDbHasuraSchema, Db, HasuraClient } from '@cardano-graphql/api-cardano-db-hasura'
import { buildSchema as buildGenesisSchema } from '@cardano-graphql/api-genesis'
import { GraphQLSchema } from 'graphql'

export * from './config'

export async function CompleteApiServer (config: Config): Promise<Server> {
  const schemas: GraphQLSchema[] = []
  if (config.genesisFileByron !== undefined || config.genesisFileShelley !== undefined) {
    schemas.push(buildGenesisSchema({
      ...config.genesisFileByron !== undefined ? { byron: require(config.genesisFileByron) } : {},
      ...config.genesisFileShelley !== undefined ? { shelley: require(config.genesisFileShelley) } : {}
    }))
  }
  if (config.hasuraUri !== undefined) {
    const hasuraClient = new HasuraClient(config.hasuraUri)
    const db = new Db(config.db)
    await db.init({
      onDbSetup: hasuraClient.applySchemaAndMetadata.bind(hasuraClient)
    })
    schemas.push(await buildCardanoDbHasuraSchema(hasuraClient))
  }
  return new Server(schemas, config)
}
