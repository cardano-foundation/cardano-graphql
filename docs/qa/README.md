# QA
Access the remote [mainnet](https://cardano-graphql-mainnet.daedalus-operations.com/) or [testnet](https://cardano-graphql-testnet.daedalus-operations.com/) deployments, or run a local stack: 
### System Requirements
- Docker v19+
- docker-compose 1.24+

### Launch the stack on preferred supported network
#### Start
```
docker-compose up
# OR
NETWORK=testnet docker-compose up
```
The GraphQL Playground can be found at http://localhost:3100

### Stop and cleanup
`docker-compose down -v`

### Troubleshooting
If you have a port clash on the host, change the mapping in the [docker-compose](../../docker-compose.yml)

## Test
[Interacted with the API directly](../api_consumer/interacting_with_the_api.md), or aim testing tools at the GraphQL server running at http://localhost:3100/graphql. This codebase implements a CI with end-to-end test and CD into a staging environment for both the mainnet and testnet. The [test suites](../../src/__test__/tests) are scoped to the GraphQL queries, as defined in the [schema](../../src/schema.graphql), and are required to be passing before new code is merged. 

The test runner is `jest`. To run the main test suite, run `jest suite`. To run a specific test file run `jest <testfile>`. 

