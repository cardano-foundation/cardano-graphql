# Cardano GraphQL Index Service

A Docker service for creating performance indexes on the cardano-db-sync PostgreSQL database.

## Overview

The index service creates additional database indexes to improve query performance. These indexes are **optional** but **recommended for production** because:

- `idx_ma_tx_mint_ident` is required for efficient asset polling (without it polls degrade from <1ms to 500ms+)
- `idx_tx_out_address` significantly speeds up payment address queries
- `idx_asset_fingerprint` speeds up asset fingerprint lookups

Index creation is not enabled by default because:
- Index creation can take several hours on mainnet (up to 6 hours total)
- Indexes consume additional disk space (typically 10-20% of database size)

> **Note:** Without `idx_ma_tx_mint_ident`, new asset polling still works correctly but is significantly slower. On mainnet this can reach several seconds per poll cycle.

## Quick Start

### Enable the Index Service

Add `indexes` to the `COMPOSE_PROFILES` variable in your `.env` file:

```bash
# .env.docker-compose
COMPOSE_PROFILES=token-registry,indexes
```

### Start the Service

```bash
docker compose --env-file .env.docker-compose up -d
```

The index-service will:
1. Wait for the database to be ready
2. Wait for cardano-db-sync schema to be initialized
3. Optionally wait for minimum block count (configurable)
4. Create all indexes concurrently (non-blocking)
5. Exit when complete

### Monitor Progress

```bash
# Watch index creation progress
docker compose logs -f index-service

# Check if service has completed
docker compose ps index-service
```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `INDEX_MIN_BLOCK_COUNT` | `1000` | Minimum blocks to sync before creating indexes |
| `POSTGRES_HOST` | `postgres` | Database host |
| `POSTGRES_PORT` | `5432` | Database port |
| `POSTGRES_DB` | (from secret) | Database name |
| `POSTGRES_USER` | (from secret) | Database user |
| `POSTGRES_PASSWORD` | (from secret) | Database password |

## Created Indexes

| Index | Table | Speeds up |
|-------|-------|-----------|
| `idx_ma_tx_mint_ident` | `ma_tx_mint.ident` | New asset polling (critical for background service) |
| `idx_tx_out_address` | `tx_out.address` | Payment address queries |
| `idx_asset_fingerprint` | `Asset.fingerprint` | Asset fingerprint lookups |

## Operations

### Verify Indexes Were Created

```bash
# Connect to database
docker compose exec postgres psql -U $(cat placeholder-secrets/postgres_user) \
  -d $(cat placeholder-secrets/postgres_db)

# List all custom indexes
\di idx_*

# Check index sizes
SELECT
  schemaname,
  tablename,
  indexname,
  pg_size_pretty(pg_relation_size(indexrelid)) as size
FROM pg_stat_user_indexes
WHERE indexname LIKE 'idx_%'
ORDER BY pg_relation_size(indexrelid) DESC;
```

### Restart Index Creation

If the service fails or you want to add new indexes:

```bash
# Restart the service
docker compose restart index-service

# Follow logs
docker compose logs -f index-service
```

### Remove Indexes

If you need to remove indexes to save space:

```bash
# Connect to database
docker compose exec postgres psql -U $(cat placeholder-secrets/postgres_user) \
  -d $(cat placeholder-secrets/postgres_db)

# Drop all custom indexes
DROP INDEX CONCURRENTLY IF EXISTS idx_ma_tx_mint_ident;
DROP INDEX CONCURRENTLY IF EXISTS idx_tx_out_address;
DROP INDEX CONCURRENTLY IF EXISTS idx_asset_fingerprint;
```

## Customization

### Adding Custom Indexes

To add your own indexes:

1. Edit `packages/index-service/indexes.sql`
2. Add your index using the pattern:
```sql
\echo '[X/Y] Creating index on table.column...'
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_custom_name ON table_name (column_name);
\echo '✓ Completed: idx_custom_name'
\echo ''
```
3. Rebuild and restart:
```bash
docker compose build index-service
docker compose up -d index-service
```

### Commenting Out Indexes

To disable specific indexes without removing them from the file:

1. Edit `packages/index-service/indexes.sql`
2. Comment out the unwanted `CREATE INDEX` lines with `--`
3. Rebuild and restart as above

## Troubleshooting

### Service Exits Immediately

Check logs for the reason:
```bash
docker compose logs index-service
```

Common causes:
- Database not ready (service will retry automatically)
- Schema not initialized (service waits for cardano-db-sync)
- Permission issues (check secrets are readable)

### Index Creation Fails

If an individual index fails:
1. Check disk space: `df -h`
2. Check PostgreSQL logs: `docker compose logs postgres`
3. Manually drop the failed index (if partially created)
4. Restart the service

## Best Practices

1. **Wait for Sync**: Let cardano-db-sync sync substantial data before creating indexes (use `INDEX_MIN_BLOCK_COUNT`)

2. **Monitor Resources**: Watch disk usage and I/O during index creation

3. **Production Timing**: Create indexes during low-query periods if possible (though CONCURRENTLY minimizes impact)

4. **Verify Success**: Always check that all indexes were created successfully

5. **Document Custom Indexes**: If adding your own, document their purpose and expected query improvements
