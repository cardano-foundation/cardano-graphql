# Configuration
## ENVs
### `API_PORT`: Number
Default: `3100`

### `HASURA_URI`: String
Hasura GraphQL server endpoint eg `http://localhost:8090/v1/graphql`

## `QUERY_DEPTH_LIMIT`: Number 
Default `10`
Used to set [depthLimit](https://github.com/stems/graphql-depth-limit)

## `TRACING`: Boolean
Passed as an option to [ApolloServer](https://www.apollographql.com/docs/apollo-server/api/apollo-server/)