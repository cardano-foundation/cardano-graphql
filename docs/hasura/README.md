# Hasura

[Official Documentation](https://docs.hasura.io/1.0/graphql/manual/index.html)

## Updating Metadata
When the Postgres data source or view abstractions change, you may need to update the metadata defined in `hasura/metadata.json`.

The process is:
1. Make the required metadata changes through the Hasura Console.
2. Export the new metadata through the settings utility.
3. Override the file `hasura/metadata.json` with the new version.