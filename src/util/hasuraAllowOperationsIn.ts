import { run, RunReport } from 'hasura-allow-operations-in'
import * as path from 'path'

const appSource = process.argv[2]

if (appSource === undefined) {
  throw new Error('Source path must be passed as first argument')
}

if (process.env.HASURA_URI === undefined) {
  throw new Error('HASURA_URI must be set')
}

const cardanoGraphQLOperations = path.resolve(__dirname, '..', 'graphql_operations')

const message = (result: RunReport, body: string) => {
  const operationText = result.operationsFound > 1 ? 'operations were' : 'operation was'
  console.log(`${result.operationsFound} ${operationText} ${body}`)
}

run(process.env.HASURA_URI, [`${cardanoGraphQLOperations}/**/*.graphql`, appSource], true)
  .then(result => message(result, 'found and included in the allow list'))
  .catch(error => console.error(error))
