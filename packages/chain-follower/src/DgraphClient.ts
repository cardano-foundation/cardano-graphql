import { dummyLogger } from 'ts-log'
import dgraph from 'dgraph-js'

export interface DgraphClient {
  setSchema: (schema: string) => Promise<void>
}

export const createDgraphClient = (address: string, logger = dummyLogger): DgraphClient => {
  const clientStub = new dgraph.DgraphClientStub(address)
  const dgraphClient = new dgraph.DgraphClient(clientStub)

  return {
    async setSchema (schema) {
      const op = new dgraph.Operation()
      op.setSchema(schema)
      await dgraphClient.alter(op)
      logger.info('Dgraph schema set')
    }
  }
}
