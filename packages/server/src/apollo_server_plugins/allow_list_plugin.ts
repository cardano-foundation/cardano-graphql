import { ForbiddenError } from 'apollo-server-errors'
import { PluginDefinition } from 'apollo-server-core'

export function allowListPlugin (allowList: {[key: string]: number }): PluginDefinition {
  return {
    serverWillStart (_service) {
      console.log(`The server is configured to accept ${Object.keys(allowList).length} operations from the provided allow-list`)
    },
    requestDidStart () {
      return {
        parsingDidStart (context) {
          if (allowList[context.request.query] === undefined) {
            throw new ForbiddenError('Operation is disallowed')
          }
        }
      }
    }
  }
}
