import { ConnectionConfig, createInteractionContext } from '@cardano-ogmios/client'
import { Logger } from 'ts-log'

export const createInteractionContextWithLogger = (connection: ConnectionConfig, logger: Logger, module: string, onClose?: () => Promise<void>) =>
  createInteractionContext(
    (error) => {
      logger.error({ module, error }, error.message)
    },
    async (code, reason) => {
      if (code === 1000) {
        logger.info({ module, code }, reason)
      } else {
        logger.error({ module, code }, 'Connection closed')
        await onClose?.()
      }
    },
    {
      connection
    }
  )
