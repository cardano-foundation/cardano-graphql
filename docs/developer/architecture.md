# Architecture

The GraphQL server in this codebase defines the public API, validates requests fit within the acceptable model, and then delegates resolution to [Hasura](https://hasura.io/). This service abstracts the complexity of GraphQL query resolution using a SQL database, including result mapping and real-time handling.

## Protective measures
The API currently implements two protective measures to reduce the attack surface. More sophisticated query cost associations can be added as an optimisation at a later date if required.


1. Query result sets have a `limit` and require pagination using `offset` arguments. It excluded the limit is set to `1`

2. A configurable node depth limit. `process.env.QUERY_DEPTH_LIMIT=3`

``` graphql
{
  blocks { # No limit provided, so defaults to 1
    previousBlock {
      previousBlock {
        previousBlock { ## Now invalid
           previousBlock {
             id
           }
        }
      }
    }
  }
}
```