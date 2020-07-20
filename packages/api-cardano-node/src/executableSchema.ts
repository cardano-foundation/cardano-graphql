import fs from 'fs'
import path from 'path'
import { makeExecutableSchema } from 'graphql-tools'
import { Resolvers } from './graphql_types'
import { throwIfInvalidContentType, saveReadStream } from './uploads'
import { getTip, Tip, submitTransaction } from './cli'

export function buildSchema () {
  console.log(process.env.CARDANO_NODE_SOCKET_PATH)
  // const networkArg = '--testnet-magic 42' // shelley_testnet
  // const networkArg = cardano.network === 'mainnet' ? '--mainnet' : `--testnet-magic ${cardano.networkMagic}`
  return makeExecutableSchema({
    resolvers: {
      Mutation: {
        submitTransaction: async (_root, { file }, _context) => {
          const { createReadStream, filename, mimetype, encoding } = await file
          console.log(`Submitting ${filename}, which has ${encoding} encoding`)
          throwIfInvalidContentType(mimetype)
          // cardano-cli submits a tx from disk, so pipe the multi-part upload to a tmp file
          const txFile = await saveReadStream(createReadStream())
          return submitTransaction(txFile.path).then((stdout: String) => {
            return { id: stdout }
          })
        }
      },
      Query: {
        node: () => getTip().then((tip: Tip) => {
          return {
            hash: tip.headerHash.toString(),
            number: tip.blockNo,
            slotNo: tip.slotNo
          }
        })
      }
    } as Resolvers,
    typeDefs: fs.readFileSync(path.resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
  })
}
