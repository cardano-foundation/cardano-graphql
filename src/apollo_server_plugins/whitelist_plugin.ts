import { ApolloServerPlugin } from 'apollo-server-plugin-base'
import { ForbiddenError } from 'apollo-server-errors'

export function whitelistPlugin (whitelist: {[key: string]: number }): ApolloServerPlugin {
  return {
    serverWillStart (_service) {
      console.log(`The server is configured to accept ${Object.keys(whitelist).length} operations from the provided whitelist`)
    },
    requestDidStart () {
      return {
        parsingDidStart (context) {
          if (whitelist[context.request.query] === undefined) {
            throw new ForbiddenError('Operation is forbidden')
          }
        }
      }
    }
  }
}
