import fs from 'fs'
import path from 'path'
import { exec, execSync } from 'child_process'
import { makeExecutableSchema } from 'graphql-tools'
import { Resolvers } from './graphql_types'
import { throwIfInvalidContentType } from './validation'
import tmp from 'tmp-promise'

export function buildSchema () {
  console.log(process.env.CARDANO_NODE_SOCKET_PATH)
  const networkArg = '--testnet-magic 42' // shelley_testnet
  // const networkArg = cardano.network === 'mainnet' ? '--mainnet' : `--testnet-magic ${cardano.networkMagic}`
  return makeExecutableSchema({
    resolvers: {
      Mutation: {
        submitTransaction: async (_root, { file }, _context) => {
          const { createReadStream, filename, mimetype, encoding } = await file
          console.log(`Submitting ${filename}, which has ${encoding} encoding`)
          throwIfInvalidContentType(mimetype)
          // cardano-cli submits a tx from disk, so pipe the multi-part upload to a tmp file
          const txFile = await tmp.file()
          return new Promise((resolve, reject) => {
            const writeStream = fs.createWriteStream(txFile.path)
            const readStream = createReadStream()
            readStream.pipe(writeStream)
            readStream.on('end', () => {
              // Todo: Make async, just simplifying initially
              try {
                const stdout = execSync(`cardano-cli shelley transaction submit --tx-file ${txFile.path} ${networkArg}`)
                // Todo: parse result (shape is unknown)
                return resolve({ id: stdout.toString() })
              } catch (error) {
                return reject(error)
              }
            })
          })
        }
      },
      Query: {
        node: () => new Promise((resolve, reject) => {
          return exec(
            `cardano-cli shelley query tip ${networkArg}`,
            (error, stdout, stderr) => {
              if (error !== undefined) {
                reject(error)
              } else if (stderr.toString() !== '') {
                reject(new Error(stderr.toString()))
              }
              console.log(stdout)
              // const valueLines = stdout.split('\n')
              // if (valueLines.length < 7) {
              //   return reject(new Error('Unexpected response from the node'))
              // }
              // const [hash, slotNo, number] = valueLines
              //   .slice(3, 6)
              //   .map((line) => line.split(':')[1].trim())

              // Return static values to keep tsc happy
              return resolve({
                hash: '123',
                // number: parseInt(number),
                number: 456,
                // slotNo: parseInt(slotNo)
                slotNo: 789
              })
            }
          )
        })
      }
    } as Resolvers,
    typeDefs: fs.readFileSync(path.resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
  })
}
