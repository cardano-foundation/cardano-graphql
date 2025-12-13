export type QueryPriority = 'CIP_68' | 'CIP_26'

export interface AssetV2 {
  subject: string
  metadata: AssetMetadata
  queryPriority: QueryPriority[]
}

export interface AssetMetadata {
  decimals: {
    value: number
  }
  description?: {
    value: string
  }
  logo?: {
    value: string
  }
  name?: {
    value: string
  }
  preImage?: {
    value: string
    hashFn: string
  }
  ticker?: {
    value: string
  }
  url?: {
    value: string
  }
}
