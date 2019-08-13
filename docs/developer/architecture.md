# Architecture

## Data access 
The _data loader_ pattern enables optimized retrieval from storage from a granualar field-level query. It works by passing load requests to a batching function at the end of the request, where a minimal set of operations to fulfil the request can be executed. This solves the N+1 problem

## Application-wide caching
Caching of the full response from the data store can be implemented within a _data loader_ batch function, applied on a case-by-case basis depending on the known state of data, ttl, or other external trigger. There are currently no cached requests implemented.

## Data Source
A query model that encapsulates loading logic for use within GraphQL resolvers. Each request is assigned a new set of _data source_ instances, containing references to any application-wide caches.
