import { graphql } from 'graphql'
import channels from './channels'

const { NODE_STATUS_CHANGED } = channels

const publish = (schema, pubSub) => {

  // Todo: Switch to using the status field from the response once following is resolved
  //  https://github.com/yarax/swagger-to-graphql/issues/86

  let lastState;
  const intervals = [];
  intervals.push(
    setInterval(async () => {
      const result = await graphql(schema, `query {
          get_api_v1_node_info {
              data {
                  syncProgress {
                      quantity
                  }
              }
          }
      }`)
      const { errors } = result;
      const status = !errors ? 'success' : 'failed'
      if (status !== lastState) {
        const connected = status === 'success'
        pubSub.publish(NODE_STATUS_CHANGED, { nodeStatus: { connected } })
        lastState = status
      }
    }, 2000)
  )
  return () => intervals.forEach((interval) => clearInterval(interval))
}

export default publish
