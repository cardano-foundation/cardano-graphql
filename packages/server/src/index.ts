import { createLogger } from 'bunyan'
import { getConfig } from './config'
import {
  AlonzoGenesis,
  buildSchema as buildCardanoDbHasuraSchema,
  ByronGenesis,
  CardanoNodeClient,
  Genesis,
  HasuraClient,
  ShelleyGenesis
} from '@cardano-graphql/api-cardano-db-hasura'
import { errors } from '@cardano-graphql/util'
import onDeath from 'death'
import { GraphQLSchema } from 'graphql'
import path from 'path'
import { Logger } from 'ts-log'
import { Server } from './Server'

export * from './config'

(async function () {
  const config = await getConfig()
  const logger: Logger = createLogger({
    name: 'cardano-graphql',
    level: config.loggerMinSeverity
  })
  try {
    const loadGenesis = (eraName: string) =>
      require(
        path.resolve(
          path.dirname(config.cardanoNodeConfigPath),
          cardanoNodeConfig[`${eraName}GenesisFile`]
        )
      )

    const schemas: GraphQLSchema[] = []
    const cardanoNodeConfig = require(config.cardanoNodeConfigPath)
    const genesis: Genesis = {
      alonzo: loadGenesis('Alonzo') as AlonzoGenesis,
      byron: loadGenesis('Byron') as ByronGenesis,
      shelley: loadGenesis('Shelley') as ShelleyGenesis
    }

    const cardanoNodeClient = new CardanoNodeClient(
      logger
    )
    const hasuraClient = new HasuraClient(
      config.hasuraUri,
      config.pollingInterval.adaSupply,
      logger
    )
    const server = new Server(schemas, config, logger)
    schemas.push(await buildCardanoDbHasuraSchema(hasuraClient, genesis, cardanoNodeClient))
    await cardanoNodeClient.initialize(config.ogmios)
    try {
      await server.init()
      await hasuraClient.initialize()
      await server.start()
    } catch (error) {
      logger.error(error.message)
      if (error instanceof errors.HostDoesNotExist) {
        process.exit(1)
      }
    }
    onDeath(async () => {
      await Promise.all([
        hasuraClient.shutdown,
        cardanoNodeClient.shutdown
      ])
      await server.shutdown()
      process.exit(1)
    })
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
