import { Config } from './config'
import { Server } from './Server'
import { buildSchema as buildCardanoDbHasuraSchema, Db } from '@cardano-graphql/api-cardano-db-hasura'
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
    const db = new Db(config.hasuraUri)
    await db.init()
    schemas.push(await buildCardanoDbHasuraSchema(config.hasuraUri, db))
  }
  return new Server(schemas, config)
}
