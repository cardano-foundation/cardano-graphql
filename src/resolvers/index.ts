import { scalarResolvers } from './scalar_resolvers'
import { hasuraResolvers } from './hasura_resolvers'
import { Resolvers } from '../graphql_types'

export default Object.assign({}, scalarResolvers, hasuraResolvers) as Resolvers
