import { ApolloClient, gql, InMemoryCache, NormalizedCacheObject } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'

export class DB {
  hasuraClient: ApolloClient<NormalizedCacheObject>

  constructor (hasuraUri: string) {
    this.hasuraClient = new ApolloClient({
      cache: new InMemoryCache({
        addTypename: false
      }),
      defaultOptions: {
        query: {
          fetchPolicy: 'network-only'
        }
      },
      link: createHttpLink({
        uri: `${hasuraUri}/v1/graphql`,
        fetch,
        headers: {
          'X-Hasura-Role': 'cardano-graphql'
        }
      })
    })
  }

  async getMeta () {
    const result = await this.hasuraClient.query({
      query: gql`query {
          cardano {
              tip {
                  slotNo
              }
              slotDuration
              startTime
          }}`
    })
    const { tip, slotDuration, startTime } = result.data?.cardano[0]
    const networkTipSlotNo = Math.round((Date.now() - new Date(`${startTime}Z`).getTime()) / slotDuration)
    const slotDiffFromNetworkTip = tip.slotNo - networkTipSlotNo
    return {
      initialized: slotDiffFromNetworkTip >= -20,
      slotDiffFromNetworkTip,
      syncPercentage: (tip.slotNo / networkTipSlotNo) * 100
    }
  }
}
