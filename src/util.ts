import * as fs from 'fs'
import * as path from 'path'
import gql from 'graphql-tag'
import { DocumentNode } from 'graphql'
import { FailedAttemptError } from 'p-retry'

export async function loadQueryNode (filePath: string): Promise<DocumentNode> {
  return gql`${await fs.promises.readFile(filePath)}`
}

export async function loadExampleQueryNode (queryType: string, name: string): Promise<DocumentNode> {
  return loadQueryNode(path.resolve(__dirname, 'example_queries', queryType, `${name}.graphql`))
}

export const onFailedAttemptFor = (operation: string) => ({ attemptNumber, message, retriesLeft }: FailedAttemptError) => {
  const nextAction = retriesLeft > 0 ? 'retying...' : 'exiting'
  console.log(`${operation}: Attempt ${attemptNumber} of ${attemptNumber + retriesLeft}, ${nextAction}`)
  if (retriesLeft === 0) {
    console.error(message)
    process.exit(1)
  }
}
