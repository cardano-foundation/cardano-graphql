import { FailedAttemptError } from 'p-retry'
import { dummyLogger, Logger } from 'ts-log'

export const onFailedAttemptFor = (operation: string, logger: Logger = dummyLogger) => ({ attemptNumber, message, retriesLeft }: FailedAttemptError) => {
  const nextAction = retriesLeft > 0 ? 'retrying...' : 'exiting'
  logger.warn(message)
  logger.info(`${operation}: Attempt ${attemptNumber} of ${attemptNumber + retriesLeft}, ${nextAction}`)
  if (retriesLeft === 0) {
    logger.error(message)
    process.exit(1)
  }
}
