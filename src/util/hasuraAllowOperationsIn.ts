import { run } from 'hasura-allow-operations-in'
import * as path from 'path'

const appSource = process.argv[2]

if (appSource === undefined) {
  throw new Error('Source path must be passed as first argument')
}

if (process.env.HASURA_URI === undefined) {
  throw new Error('HASURA_URI must be set')
}

const cardanoGraphQLOperations = path.resolve(__dirname, '..', 'graphql_operations')

run(process.env.HASURA_URI, [`${cardanoGraphQLOperations}/**/*.graphql`, appSource], true)
  .then(
    ({
      introspectionAllowed,
      operationDefinitionsFound,
      addedCount,
      existingCount
    }) => {
      console.log(
        `Introspection allowed: ${introspectionAllowed} | Found: ${operationDefinitionsFound.length} | Added: ${addedCount} | Existing: ${existingCount}`
      )
      if (process.env.DEBUG) {
        operationDefinitionsFound.forEach(def =>
          console.log(`${def.operation}: ${def.name.value}`)
        )
      }
    }
  )
  .catch(error => console.error(error))
