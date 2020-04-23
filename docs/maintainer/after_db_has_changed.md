# After Changes to DB Schema
## 1. Resync local stack
```
docker-compose down -v
docker-compose up
```
## 2. Update Hasura if required
[Official Documentation](https://docs.hasura.io/1.0/graphql/manual/index.html)
- Make the required metadata changes through the Hasura Console.
- Export the new metadata through the settings utility `hasura/metadata.json`

## 3. Test 
```
yarn test:e2e
```
