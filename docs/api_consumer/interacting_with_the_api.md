# Interacting with the API 
The API is served over TCP, leveraging the existing infrastructure of HTTP. GraphQL adds a powerful query layer to give you complete control over the request, including the shape of the data in the response. Detailed below are some approaches for connecting and interacting with the API, however there may be other ways more suited for your use-case.

### Command Line
```
curl -X POST \
-H "Content-Type: application/json" \
-d '{"query": "{ ledgerStats { blockHeight }}"}' \
http://localhost:4000/graphql
```

### Request within app
``` javascript
const query = `
  query getTransactions($limit: Int!, $offset: Int!) {
     transactions(
      limit: $limit,
      offset: $offset,
      where: {
        block: {
          number: {
            _gte: 50
            _lt: 100
          }
        }
      }
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

fetch('/graphql', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
  },
  body: JSON.stringify({
    query,
    variables: { limit, offset },
  })
})
  .then(r => r.json())
  .then(data => console.log(`Page ${offset} containing ${limit} transactions:', data))
```

#### Stateful client 
A full example application will demonstrate this.