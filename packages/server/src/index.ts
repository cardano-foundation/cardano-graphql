import { createLogger } from 'bunyan'
import { PointOrOrigin } from '@cardano-ogmios/schema'
import { getConfig } from './config'
import {
  AlonzoGenesis,
  buildSchema as buildCardanoDbHasuraSchema,
  ByronGenesis,
  CardanoNodeClient,
  ChainFollower,
  Genesis,
  HasuraClient,
  MetadataClient,
  ShelleyGenesis,
  Worker
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
    const chainFollower = new ChainFollower(
      hasuraClient,
      logger,
      config.db
    )
    const metadataClient = new MetadataClient(
      config.metadataServerUri,
      logger
    )
    const worker = new Worker(
      hasuraClient,
      logger,
      metadataClient,
      config.db,
      {
        metadataUpdateInterval: {
          assets: config.metadataUpdateInterval?.assets
        }
      }
    )
    const server = new Server(schemas, config, logger)
    schemas.push(await buildCardanoDbHasuraSchema(hasuraClient, genesis, cardanoNodeClient))
    const getChainSyncPoints = async (): Promise<PointOrOrigin[]> => {
      const mostRecentPoint = await hasuraClient.getMostRecentPointWithNewAsset()
      return mostRecentPoint !== null ? [mostRecentPoint, 'origin'] : ['origin']
    }
    await cardanoNodeClient.initialize(config.ogmios)
    try {
      await server.init()
      await hasuraClient.initialize()
      await metadataClient.initialize()
      await chainFollower.initialize(config.ogmios, getChainSyncPoints)
      await worker.start()
      await chainFollower.start(await getChainSyncPoints())
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
        cardanoNodeClient.shutdown,
        worker.shutdown,
        chainFollower.shutdown
      ])
      await server.shutdown()
      process.exit(1)
    })
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
