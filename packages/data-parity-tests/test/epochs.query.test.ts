import gql from 'graphql-tag'
import utilDev, { TestClient } from '@cardano-graphql/util-dev'
import { getDataFromAPI } from '@src/util'

describe('epochs ', () => {
  let client: TestClient
  let restData: any
  let graphQLData: any
  const epoch = 111
  const slot = 3313

  beforeAll(async () => {
    process.env.CARDANO_GRAPHQL_URI = '3201'
    client = await utilDev.createE2EClient()
    const restResult = await getDataFromAPI(`epochs/${epoch}/${slot}`)
    restData = restResult.Right[0]
    const graphQLResult = await client.query({
      query: gql`query EpochDetails{
          epochs(where: {number:{_eq:${epoch}}}){
                          number
                          startedAt
                          lastBlockTime
              blocks (where: {slotInEpoch:{_eq:${slot}}}){
                            slotNo
                            epochNo
                            slotInEpoch
                            number
                            hash
                            forgedAt
                            transactionsCount
                            transactions{
                              totalOutput
                            }
                            size
                            createdBy
                            fees
                          }
                          output
                        }
                      }`
    })
    graphQLData = graphQLResult.data.epochs[0].blocks[0]
  }, 30000)

  it('return the same epoch number', async () => {
    const restResultEpochNumber = restData.cbeEpoch
    const graphQLEpochNumber = graphQLData.epochNo

    expect(restResultEpochNumber).toEqual(graphQLEpochNumber)
  })

  it('return the same slot number', async () => {
    const restResultSlotNumber = restData.cbeSlot
    const graphQLSlotNumber = graphQLData.slotInEpoch

    expect(restResultSlotNumber).toEqual(graphQLSlotNumber)
  })

  it('return the same block height', async () => {
    const restResultBlockHeight = restData.cbeBlkHeight
    const graphQLBlockHeight = graphQLData.number

    expect(restResultBlockHeight).toEqual(graphQLBlockHeight)
  })

  it('return the same block hash', async () => {
    const restResultBlockHash = restData.cbeBlkHash
    const graphQLBlockHash = graphQLData.id

    expect(restResultBlockHash).toEqual(graphQLBlockHash)
  })

  it('return the same block creation time', async () => {
    const restResultBlockCreationUnixEpochTime = restData.cbeTimeIssued
    const graphQLBlockCreationDateTime = graphQLData.forgedAt
    const restResultBlockCreationDateTime = utilDev.timestampToIsoStringWithoutTimezone(restResultBlockCreationUnixEpochTime)

    expect(restResultBlockCreationDateTime).toEqual(graphQLBlockCreationDateTime)
  })

  it('return the same transactions count', async () => {
    const restResultTxCount = restData.cbeTxNum
    const graphQLTxCount = parseInt(graphQLData.transactionsCount)

    expect(restResultTxCount).toEqual(graphQLTxCount)
  })

  it('return the same total output', async () => {
    const restResultTotalSent = parseInt(restData.cbeTotalSent.getCoin)
    let graphQLTotalSent = 0

    graphQLData.transactions.forEach(
      (tx: any) => {
        graphQLTotalSent += parseInt(tx.totalOutput)
      }
    )

    expect(restResultTotalSent).toEqual(graphQLTotalSent)
  })

  it('return the same block size', async () => {
    const restResultBlockSize = restData.cbeSize
    const graphQLBlockSize = graphQLData.size

    expect(restResultBlockSize).toEqual(graphQLBlockSize)
  })

  it('return the same block leader', async () => {
    const restResultBlockLeader = restData.cbeBlockLead
    const graphQLBlockLeader = graphQLData.createdBy.split('-')[1]

    expect(restResultBlockLeader).toMatch(graphQLBlockLeader)
  })

  it('return the same fee', async () => {
    const restResultFee = parseInt(restData.cbeFees.getCoin)
    const graphQLFee = graphQLData.fees

    expect(restResultFee).toEqual(graphQLFee)
  })
})
