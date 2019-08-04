# Interacting with the API 
The API is usually served over TCP, leveraging the existing infrastructure of HTTP. GraphQL adds a powerful query layer to what you may be used to, exposing a more logical data model and providing opportunity for tailored results optimized on the client-side. Detailed below are some approaches for connecting and interacting with the API, however there may be other ways more suited for your use-case.

### Command Line
```
curl -X POST \
-H "Content-Type: application/json" \
-d '{"query": "{ hello }"}' \
http://localhost:4000/graphql
```

### Request within app
```
var dice = 3;
var sides = 6;
var query = `query RollDice($dice: Int!, $sides: Int) {
  rollDice(numDice: $dice, numSides: $sides)
}`;

fetch('/graphql', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
  },
  body: JSON.stringify({
    query,
    variables: { dice, sides },
  })
})
  .then(r => r.json())
  .then(data => console.log('data returned:', data));
```

#### Stateful client 
Todo: Apollo client example, with and without use of React components