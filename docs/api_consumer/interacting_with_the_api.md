# Interacting with the API 
The API is served over TCP, leveraging the existing infrastructure of HTTP. GraphQL adds a powerful query layer to give you complete control over the request, including the shape of the data in the response. Detailed below are some approaches for connecting and interacting with the API, however there may be other ways more suited for your use-case.

### Command Line
```
➜  curl -X POST \
-H "Content-Type: application/json" \
-d '{"query": "{ cardano { blockHeight }}"}' \
http://localhost:3100/graphql  

{"data":{"cardano":{"blockHeight":70205}}}

```

### Request within app
``` javascript
const query = `
  query getTransactions(
    $limit: Int,
    $offset: Int,
    $where: Transaction_bool_exp
  ) {
     transactions(
      limit: $limit,
      offset: $offset,
      where: $where
    ) {
      fee
      block {
        number
      }
      id
    }
  }`

const limit = 100
const offset = 2
const where = {
  block: {
    number: {
      _gte: 50
      _lt: 100
    }
  }
}

fetch('/graphql', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
  },
  body: JSON.stringify({
    query,
    variables: { limit, offset, where },
  })
})
  .then(r => r.json())
  .then(data => console.log(`Page ${offset} containing ${limit} transactions:', data))
```

#### Stateful client 
A full example application will demonstrate this.