import { createLogger } from 'bunyan'
import { Schema } from '@cardano-ogmios/client'
import { getConfig } from './config'
import {
  buildSchema as buildCardanoDbHasuraSchema,
  CardanoNodeClient,
  ChainFollower,
  Db,
  Genesis,
  HasuraClient,
  Worker
} from '@cardano-graphql/api-cardano-db-hasura'
import { errors, networkInfoFromMagic } from '@cardano-graphql/util'
import onDeath from 'death'
import { GraphQLSchema } from 'graphql'
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
    const chainFollower = new ChainFollower(
      hasuraClient,
      logger,
      config.db
    )
    const worker = new Worker(
      hasuraClient,
      logger,
      config.metadataServerUri,
      config.db,
      {
        metadataUpdateInterval: {
          assets: config.metadataUpdateInterval?.assets
        }
      }
    )
    await db.init({
      onDbSetup: async () => {
        try {
          await hasuraClient.initialize()
          await cardanoNodeClient.initialize(config.ogmios)
          await worker.initialize()
          await chainFollower.initialize(config.ogmios)
          const mostRecentPoint = await hasuraClient.getMostRecentPointWithNewAsset()
          const points: Schema.Point[] = mostRecentPoint !== null ? [mostRecentPoint] : []
          points.push(
            networkInfoFromMagic(genesis.shelley.networkMagic).eras.allegra.lastPoint,
            'origin'
          )
          await worker.start()
          await chainFollower.start(points)
        } catch (error) {
          logger.error(error.message)
          if (error instanceof errors.HostDoesNotExist) {
            process.exit(1)
          }
        }
      }
    })
    schemas.push(await buildCardanoDbHasuraSchema(hasuraClient, genesis, cardanoNodeClient))
    const server = new Server(schemas, config, logger)
    await server.init()
    onDeath(() => {
      Promise.all([
        server.shutdown,
        chainFollower.shutdown,
        worker.shutdown,
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
