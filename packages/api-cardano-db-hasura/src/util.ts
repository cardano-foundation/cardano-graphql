import { ConnectionConfig, createInteractionContext } from '@cardano-ogmios/client'
import { Logger } from 'ts-log'

export const createInteractionContextWithLogger = (connection: ConnectionConfig, logger: Logger, module: string) =>
  createInteractionContext(
    (error) => {
      logger.error({ module, error }, error.message)
    },
    (code, reason) => {
      if (code === 1006) {
        logger.error({ module, code }, 'Connection was closed abnormally')
      } else {
        logger.info({ module, code }, reason)
      }
    },
    {
      connection,
      interactionType: 'LongRunning'
    }
  )
