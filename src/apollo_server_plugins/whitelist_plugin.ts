import { ApolloServerPlugin } from 'apollo-server-plugin-base'

export function whitelistPlugin (whitelist: {[key: string]: number }): ApolloServerPlugin {
  return {
    serverWillStart () {
      console.log(`The server is configured to accept ${Object.keys(whitelist).length} operations from the provided whitelist`)
    },
    requestDidStart (context) {
      if (whitelist[context.request.query] === undefined) {
        throw new Error('Operations is not in whitelist')
      }
    }
  }
}
