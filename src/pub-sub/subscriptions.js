import gql from 'graphql-tag'
import channels from './channels';
import { makeExecutableSchema } from 'graphql-tools'

const { NODE_STATUS_CHANGED } = channels;

const typeDefs = [
  gql`
      type NodeStatus {
          connected: Boolean!
      }
      type Subscription {
          nodeStatus: NodeStatus
      }
      type Query {
          ping: String
      }
  `]

export default (pubSub) => {
  const resolvers = {
    Subscription: {
      nodeStatus: {
        subscribe: () => pubSub.asyncIterator(NODE_STATUS_CHANGED)
      }
    }
  }
  return makeExecutableSchema({ typeDefs, resolvers })
}
