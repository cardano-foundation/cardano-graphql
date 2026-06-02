import { exec } from 'child_process'
import util, { ModuleState } from '@cardano-graphql/util'
import { GraphQLSchema } from 'graphql'
import { GraphQLClient, gql } from 'graphql-request'
import { Client } from 'pg'
import pRetry from 'p-retry'
import path from 'path'
import { dummyLogger, Logger } from 'ts-log'
import { Asset, Block } from './graphql_types'
import {
  AssetMetadataAndHash,
  AssetMetadataHashAndId,
  AssetWithoutTokens,
  DbConfig
} from './typeAliases'
const epochInformationNotYetAvailable =
  'Epoch information not yet available. This is expected during the initial chain-sync.'

const withHexPrefix = (value: string) =>
  `\\x${value !== undefined ? value : ''}`

export class HasuraBackgroundClient {
  private client: GraphQLClient
  private applyingSchemaAndMetadata: boolean
  private state: ModuleState
  public schema: GraphQLSchema

  constructor (
    readonly hasuraCliPath: string,
    readonly hasuraCliExtPath: string,
    readonly hasuraUri: string,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.applyingSchemaAndMetadata = false
    this.client = new GraphQLClient(`${this.hasuraUri}/v1/graphql`, {
      headers: {
        'X-Hasura-Role': 'cardano-graphql'
      }
    })
  }

  private async hasuraCli (command: string) {
    return new Promise((resolve, reject) => {
      exec(
        `${this.hasuraCliPath} --cli-ext-path ${this.hasuraCliExtPath} --skip-update-check --project ${path.resolve(__dirname, '..', 'hasura', 'project')} --endpoint ${this.hasuraUri} ${command}`,
        (error, stdout) => {
          if (error) {
            reject(error)
          }
          if (stdout !== '') { this.logger.debug({ module: 'HasuraBackgroundClient' }, stdout) }
          resolve({ module: 'HasuraBackgroundClient' })
        }
      )
    })
  }

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: 'HasuraBackgroundClient' }, 'Initializing')
    await pRetry(
      async () => {
        const result = await this.client.request(gql`
          query {
            epochs(limit: 1, order_by: { number: desc }) {
              number
            }
          }
        `)
        if (result.epochs.length === 0) {
          this.logger.debug(
            { module: 'HasuraBackgroundClient' },
            epochInformationNotYetAvailable
          )
          throw new Error(epochInformationNotYetAvailable)
        }
      },
      {
        factor: 1.05,
        retries: 10,
        onFailedAttempt: util.onFailedAttemptFor(
          'Detecting DB sync state has reached minimum progress',
          this.logger
        )
      }
    )
    this.logger.debug(
      { module: 'HasuraBackgroundClient' },
      'DB sync state has reached minimum progress'
    )
    this.state = 'initialized'
    this.logger.info({ module: 'HasuraBackgroundClient' }, 'Initialized')
  }

  public async shutdown () {
    this.state = null
  }

  public async applySchemaAndMetadata (): Promise<void> {
    if (this.applyingSchemaAndMetadata) return
    this.applyingSchemaAndMetadata = true
    this.logger.info({ module: 'HasuraBackgroundClient' }, 'Applying PostgreSQL schema migrations')
    await pRetry(
      async () => {
        await this.hasuraCli('migrate --database-name default apply --down all')
        await this.hasuraCli('migrate --database-name default apply --up all')
      },
      {
        factor: 1.75,
        retries: 9,
        onFailedAttempt: util.onFailedAttemptFor(
          'Applying PostgreSQL schema migrations',
          this.logger
        )
      }
    )
    this.logger.info({ module: 'HasuraBackgroundClient' }, 'Applying Hasura metadata')
    await pRetry(
      async () => {
        await this.hasuraCli('metadata clear')
        await this.hasuraCli('metadata apply')
      },
      {
        factor: 1.75,
        retries: 9,
        onFailedAttempt: util.onFailedAttemptFor(
          'Applying Hasura metadata',
          this.logger
        )
      }
    )
    this.logger.info({ module: 'HasuraBackgroundClient' }, 'Hasura setup complete')
    this.applyingSchemaAndMetadata = false
  }

  public async deleteAssetsAfterSlot (slotNo: Block['slotNo']): Promise<number> {
    this.logger.debug(
      { module: 'HasuraClient', slotNo },
      'deleting assets found in tokens after slot'
    )

    const result = await this.client.request(
      gql`
        mutation DeleteAssetsAfterSlot($slotNo: Int!) {
          delete_assets(where: { firstAppearedInSlot: { _gt: $slotNo } }) {
            affected_rows
          }
        }
      `,
      {
        slotNo
      }
    )
    return result.delete_assets.affected_rows
  }

  public async hasAsset (assetId: Asset['assetId']): Promise<boolean> {
    const result = await this.client.request(
      gql`
        query HasAsset($assetId: bytea!) {
          assets(where: { assetId: { _eq: $assetId } }) {
            assetId
          }
        }
      `,
      {
        assetId: withHexPrefix(assetId)
      }
    )
    const response = result.assets.length > 0
    this.logger.debug(
      { module: 'HasuraClient', assetId, hasAsset: response },
      'Has asset?'
    )
    return response
  }

  public async addAssetMetadata (asset: AssetMetadataAndHash) {
    this.logger.info(
      { module: 'HasuraClient', assetId: asset.assetId },
      'Adding metadata to asset'
    )
    const result = await this.client.request(
      gql`
        mutation AddAssetMetadata(
          $assetId: bytea!
          $decimals: Int
          $description: String
          $logo: String
          $metadataHash: bpchar!
          $name: String
          $ticker: String
          $url: String
        ) {
          update_assets(
            where: { assetId: { _eq: $assetId } }
            _set: {
              decimals: $decimals
              description: $description
              logo: $logo
              metadataHash: $metadataHash
              name: $name
              ticker: $ticker
              url: $url
            }
          ) {
            affected_rows
            returning {
              assetId
            }
          }
        }
      `,
      {
        ...asset,
        assetId: withHexPrefix(asset.assetId),
        ticker: asset.ticker && asset.ticker.length > 9 ? undefined : asset.ticker
      }
    )
    if (result.errors !== undefined) {
      throw new Error(result.errors)
    }
  }

  public async insertAssets (assets: AssetWithoutTokens[]) {
    this.logger.debug(
      { module: 'HasuraClient', qty: assets.length },
      'inserting assets found in tokens'
    )
    const result = await this.client.request(
      gql`
        mutation InsertAssets($assets: [Asset_insert_input!]!) {
          insert_assets(
            objects: $assets
            on_conflict: { constraint: Asset_pkey, update_columns: [] }
          ) {
            returning {
              name
              policyId
              description
              assetName
              assetId
            }
            affected_rows
          }
        }
      `,
      {
        assets: assets.map((asset) => ({
          ...asset,
          ...{
            assetId: withHexPrefix(asset.assetId),
            assetName: withHexPrefix(asset.assetName),
            policyId: withHexPrefix(asset.policyId)
          }
        }))
      }
    )
    return result
  }

  public async getAssetMetadataHashesById (
    assetIds: Asset['assetId'][]
  ): Promise<AssetMetadataHashAndId[]> {
    const result = await this.client.request(
      gql`
        query AssetMetadataHashes($assetIds: [bytea!]!) {
          assets(where: { assetId: { _in: $assetIds } }) {
            assetId
            metadataHash
          }
        }
      `,
      {
        assetIds: assetIds.map((id) => withHexPrefix(id))
      }
    )
    return result.assets
  }

  public async getMaxMultiAssetId (dbConfig: DbConfig): Promise<number> {
    const client = new Client(dbConfig)
    await client.connect()
    try {
      const result = await client.query<{ maxId: string }>('SELECT COALESCE(MAX(id), 0) AS "maxId" FROM multi_asset')
      return Number(result.rows[0].maxId)
    } finally {
      await client.end()
    }
  }

  public async pollNewAssets (dbConfig: DbConfig, lastSeenId: number): Promise<{ assetIds: string[], nextLastSeenId: number }> {
    const client = new Client(dbConfig)
    await client.connect()
    try {
      const boundaryResult = await client.query<{ maxConfirmedId: string }>(`
        SELECT COALESCE(MAX(ma.id), $1) AS "maxConfirmedId"
        FROM multi_asset ma
        JOIN ma_tx_mint mtm ON mtm.ident = ma.id
        JOIN tx             ON tx.id     = mtm.tx_id
        JOIN block b        ON b.id      = tx.block_id
        WHERE ma.id > $1
          AND b.slot_no < (SELECT slot_no FROM block ORDER BY id DESC LIMIT 1) - 120
      `, [lastSeenId])

      const nextLastSeenId = Number(boundaryResult.rows[0].maxConfirmedId)

      if (nextLastSeenId === lastSeenId) {
        return { assetIds: [], nextLastSeenId }
      }

      const result = await client.query<{ assetId: string }>(`
        INSERT INTO "Asset" ("assetId", "assetName", "policyId", "fingerprint", "firstAppearedInSlot")
        SELECT
          CAST(CONCAT(ma.policy, RIGHT(CONCAT(E'\\\\', ma.name), -3)) AS BYTEA),
          ma.name,
          ma.policy,
          ma.fingerprint,
          MIN(b.slot_no)
        FROM multi_asset ma
        JOIN ma_tx_mint mtm ON mtm.ident = ma.id
        JOIN tx             ON tx.id     = mtm.tx_id
        JOIN block b        ON b.id      = tx.block_id
        LEFT JOIN "Asset" a
          ON a."assetId" = CAST(CONCAT(ma.policy, RIGHT(CONCAT(E'\\\\', ma.name), -3)) AS BYTEA)
        WHERE a."assetId" IS NULL
          AND ma.id > $1
          AND ma.id <= $2
        GROUP BY ma.id, ma.policy, ma.name, ma.fingerprint
        ON CONFLICT ("assetId") DO NOTHING
        RETURNING encode("assetId", 'hex') AS "assetId"
      `, [lastSeenId, nextLastSeenId])

      if (result.rowCount > 0) {
        this.logger.info(
          { module: 'HasuraBackgroundClient', inserted: result.rowCount },
          'Polled and inserted new assets from multi_asset'
        )
      }

      return {
        assetIds: result.rows.map((row) => row.assetId),
        nextLastSeenId
      }
    } finally {
      await client.end()
    }
  }

  public async getAssetIdsWithoutMetadata (dbConfig: DbConfig): Promise<string[]> {
    const client = new Client(dbConfig)
    await client.connect()
    try {
      const result = await client.query<{ assetId: string }>(`
        SELECT encode("assetId", 'hex') AS "assetId"
        FROM "Asset"
        WHERE "metadataHash" IS NULL
      `)
      this.logger.info(
        { module: 'HasuraBackgroundClient', qty: result.rowCount },
        'Found assets without metadata'
      )
      return result.rows.map(row => row.assetId)
    } finally {
      await client.end()
    }
  }

  public async getRecentAssetIdsWithoutMetadata (dbConfig: DbConfig): Promise<string[]> {
    const NINETY_DAYS_IN_SLOTS = 7_776_000
    const client = new Client(dbConfig)
    await client.connect()
    try {
      const result = await client.query<{ assetId: string }>(`
        SELECT encode("assetId", 'hex') AS "assetId"
        FROM "Asset"
        WHERE "metadataHash" IS NULL
        AND "firstAppearedInSlot" > (SELECT MAX("firstAppearedInSlot") FROM "Asset") - $1
      `, [NINETY_DAYS_IN_SLOTS])
      this.logger.info(
        { module: 'HasuraBackgroundClient', qty: result.rowCount },
        'Found recent assets without metadata'
      )
      return result.rows.map(row => row.assetId)
    } finally {
      await client.end()
    }
  }

  public async backfillMissingAssets (dbConfig: DbConfig): Promise<string[]> {
    const client = new Client(dbConfig)
    await client.connect()
    try {
      await client.query(`
        UPDATE "Asset" a
        SET fingerprint = ma.fingerprint
        FROM multi_asset ma
        WHERE a."assetId" = CAST(CONCAT(ma.policy, RIGHT(CONCAT(E'\\\\', ma.name), -3)) AS BYTEA)
          AND a.fingerprint IS NULL
      `)
      const result = await client.query(`
        INSERT INTO "Asset" ("assetId", "assetName", "policyId", "fingerprint", "firstAppearedInSlot")
        SELECT
          CAST(CONCAT(ma.policy, RIGHT(CONCAT(E'\\\\', ma.name), -3)) AS BYTEA),
          ma.name,
          ma.policy,
          ma.fingerprint,
          MIN(b.slot_no)
        FROM multi_asset ma
        JOIN ma_tx_mint mtm ON mtm.ident = ma.id
        JOIN tx             ON tx.id     = mtm.tx_id
        JOIN block b        ON b.id      = tx.block_id
        LEFT JOIN "Asset" a
          ON a."assetId" = CAST(CONCAT(ma.policy, RIGHT(CONCAT(E'\\\\', ma.name), -3)) AS BYTEA)
        WHERE a."assetId" IS NULL
        GROUP BY ma.id, ma.policy, ma.name, ma.fingerprint
        ON CONFLICT ("assetId") DO NOTHING
        RETURNING encode("assetId", 'hex') AS "assetId"
      `)
      this.logger.info(
        { module: 'HasuraBackgroundClient', inserted: result.rowCount },
        'Backfilled missing assets from multi_asset'
      )
      return result.rows.map((row: { assetId: string }) => row.assetId)
    } finally {
      await client.end()
    }
  }
}
