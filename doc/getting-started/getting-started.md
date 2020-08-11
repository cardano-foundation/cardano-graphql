## Getting Started

``` console
git clone git@github.com:input-output-hk/cardano-graphql.git
cd cardano-graphql
```
### Build and Run via Docker Compose
Builds `@cardano-graphql/server` and starts it along with `cardano-node`, `cardano-db-sync-extended`, `postgresql`, and `hasura`:

``` console
docker-compose up -d --build && docker-compose logs -f
```
> NOTE:  _Omit the `--build` to use a pre-built image from Dockerhub (or locally cached from previous build)_

### Check Cardano DB sync progress

Use the GraphQL Playground in the browser at http://localhost:3100/graphql:
``` graphql 
{ cardanoDbMeta { initialized syncPercentage }}
```
or via command line:
``` console
curl -X POST -H "Content-Type: application/json" -d '{"query": "{ cardanoDbMeta { initialized syncPercentage }}"}' http://localhost:3100/graphql
```
> NOTE:  Wait for `initialized` to be `true` to ensure the epoch dataset is complete.

### Query the full dataset

```graphql
{ cardano { tip { number slotNo epoch { number } } } }
```
``` console
curl -X POST -H "Content-Type: application/json" -d '{"query": "{ cardano { tip { number slotNo epoch { number } } } }"}' http://localhost:3100/graphql
```
### :tada:

``` json
{ "data": { "cardano": { "tip": { "number": 4391749, "slotNo": 4393973, "epoch": { "number": 203 } } } } }
```

For more information, have a look at the [Wiki :book:](https://github.com/input-output-hk/cardano-graphql/wiki).