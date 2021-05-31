import { createLogger } from 'bunyan'
import { getConfig } from './config'
import {
  buildSchema as buildCardanoDbHasuraSchema,
  CardanoNodeClient,
  DataSyncController,
  Db,
  Genesis,
  HasuraClient,
  HostDoesNotExist
} from '@cardano-graphql/api-cardano-db-hasura'
import onDeath from 'death'
import { GraphQLSchema } from 'graphql'
import { Server } from './Server'
import { Logger } from 'ts-log'

export * from './config'

(async function () {
  const config = await getConfig()
  const logger: Logger = createLogger({
    name: 'cardano-graphql',
    level: config.loggerMinSeverity
  })
  try {
    const schemas: GraphQLSchema[] = []
    let genesis: Genesis
    if (config.genesis.byronPath !== undefined || config.genesis.shelleyPath !== undefined) {
      genesis = {
        ...config.genesis.byronPath !== undefined ? { byron: require(config.genesis.byronPath) } : {},
        ...config.genesis.shelleyPath !== undefined ? { shelley: require(config.genesis.shelleyPath) } : {}
      }
    }
    const lastConfiguredMajorVersion = require(config.cardanoNodeConfigPath)['LastKnownBlockVersion-Major']

    const cardanoNodeClient = new CardanoNodeClient(
      lastConfiguredMajorVersion,
      logger
    )
    const hasuraClient = new HasuraClient(
      config.hasuraCliPath,
      config.hasuraUri,
      config.pollingInterval.adaSupply,
      lastConfiguredMajorVersion,
      logger
    )
    const db = new Db(config.db, logger)
    const dataSyncController = new DataSyncController(
      hasuraClient,
      db,
      config.pollingInterval.metadataSync,
      logger,
      config.metadataServerUri
    )
    await db.init({
      onDbSetup: () =>
        hasuraClient.initialize()
          .then(() => dataSyncController.initialize())
          .catch((error) => {
            if (error instanceof HostDoesNotExist) {
              logger.error(error.message)
              process.exit(1)
            }
          })
    })
    await cardanoNodeClient.initialize(config.ogmios)
    schemas.push(await buildCardanoDbHasuraSchema(hasuraClient, genesis, cardanoNodeClient))
    const server = new Server(schemas, config, logger)
    await server.init()
    onDeath(() => {
      Promise.all([
        server.shutdown,
        dataSyncController.shutdown,
        db.shutdown,
        cardanoNodeClient.shutdown,
        hasuraClient.shutdown
      ]).then(() => process.exit(1))
    })
    await server.start()
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
