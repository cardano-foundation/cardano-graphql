# QA

## System Requirements
- Docker v19+
- docker-compose 1.24+

## Launch the _mainnet_ snapshot test stack
### Start
```
docker-compose -f test/docker-compose.yml up
```
Finding the GraphQL Playground at http://localhost:3100 verifies the env is ready

### Test
At this point the [API can be interacted with](../api_consumer/interacting_with_the_api.md), or testing tools aimed at the GraphQL server running at http://localhost:3100/graphql. This codebase implements a CI with end-to-end test and CD into a staging environment for both the mainnet and testnet. The [test suites](../../src/__test__/tests) are scoped to the GraphQL queries, as defined in the [schema](../../src/schema.graphql), and are required to be passing before new code is merged. 

### Stop and cleanup
`docker-compose -f test/docker-compose.yml down -v`

### Troubleshooting
If you have a port clash on the host, change the mapping in the [docker-compose](../../test/docker-compose.yml)