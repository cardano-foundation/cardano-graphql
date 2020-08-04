import { FailedAttemptError } from 'p-retry'

export const onFailedAttemptFor = (operation: string) => ({ attemptNumber, message, retriesLeft }: FailedAttemptError) => {
  const nextAction = retriesLeft > 0 ? 'retying...' : 'exiting'
  console.warn(message)
  console.log(`${operation}: Attempt ${attemptNumber} of ${attemptNumber + retriesLeft}, ${nextAction}`)
  if (retriesLeft === 0) {
    console.error(message)
    process.exit(1)
  }
}
