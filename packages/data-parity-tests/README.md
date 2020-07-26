# Data Parity Tests

An e2e test suite to ensure data parity across comparable services. 
Uses docker-compose to run the services

## Install
```
yarn --offline && yarn build
```
## Stand up the test stack
```
yarn services:up
```
## Run the tests
```
yarn test:run
```

## Teardown, optionally removing data volumes
```
yarn services:down
# OR
yarn services:down -v
```
