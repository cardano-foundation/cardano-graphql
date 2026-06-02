# Contributing

## Maintainer

This project is maintained by the [Cardano Foundation](https://cardanofoundation.org).
Current maintainer: [@VladislavKudrin](https://github.com/VladislavKudrin)

## Reporting Issues

Open a [GitHub issue](https://github.com/cardano-foundation/cardano-graphql/issues) with:
- Steps to reproduce
- Expected vs actual behavior
- Environment (OS, Docker version, cardano-node/db-sync versions)

## Submitting Pull Requests

1. Fork the repository
2. Create a branch from `master`
3. Make your changes
4. Ensure lint passes: `yarn workspaces run lint`
5. Open a PR against `master`

## Running Locally

See [README.md](README.md) for setup instructions.

To run tests against a live preprod node:
```bash
GRAPHQL_URL=http://localhost:<port> yarn workspace @cardano-graphql/api-cardano-db-hasura test --forceExit
```
GraphQL golden tests (requires a running mainnet node):
```bash
cd tests
uv run pytest
```

## Code of Conduct

Please follow our [Code of Conduct](CODE-OF-CONDUCT.md).
