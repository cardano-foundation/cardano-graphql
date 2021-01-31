import { Config } from './config'
import { Server } from './Server'
import {
  buildSchema as buildCardanoDbHasuraSchema,
  CardanoNodeClient,
  createCardanoCli,
  Db,
  Genesis,
  HasuraClient
} from '@cardano-graphql/api-cardano-db-hasura'
import { GraphQLSchema } from 'graphql'
import { dummyLogger, Logger } from 'ts-log'

export * from './config'

export async function CompleteApiServer (
  config: Config,
  logger: Logger = dummyLogger
): Promise<Server> {
  const schemas: GraphQLSchema[] = []
  let genesis: Genesis
  let cardanoNodeClient: CardanoNodeClient
  if (config.genesis.byronPath !== undefined || config.genesis.shelleyPath !== undefined) {
    genesis = {
      ...config.genesis.byronPath !== undefined ? { byron: require(config.genesis.byronPath) } : {},
      ...config.genesis.shelleyPath !== undefined ? { shelley: require(config.genesis.shelleyPath) } : {}
    }
  }
  if (config.cardanoCliPath !== undefined) {
    cardanoNodeClient = new CardanoNodeClient(
      createCardanoCli(config.cardanoCliPath, genesis.shelley, config.jqPath),
      1000 * 60 * 10,
      genesis.shelley.protocolParams.protocolVersion.major,
      logger
    )
  }
  if (config.hasuraUri !== undefined) {
    const hasuraClient = new HasuraClient(
      config.hasuraCliPath,
      config.hasuraUri,
      config.pollingIntervalAdaSupply,
      logger
    )
    const db = new Db(config.db, logger)
    await db.init({
      onDbSetup: () => Promise.all([
        hasuraClient.initialize(),
        cardanoNodeClient.initialize()
      ])
    })
    schemas.push(await buildCardanoDbHasuraSchema(hasuraClient, genesis, cardanoNodeClient))
  }
  return new Server(schemas, config, logger)
}
