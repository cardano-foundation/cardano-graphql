
export interface Signature {
  signature: string
  publicKey: string
}

export interface AssetMetadata {
  decimals: {
    value: number
    anSignatures: Signature[]
  }
  description?: {
    value: string
    anSignatures: Signature[]
  }
  logo?: {
    value: string
    anSignatures: Signature[]
  }
  name?: {
    value: string
    anSignatures: Signature[]
  }
  owner?: Signature
  preImage?: {
    value: string
    hashFn: string
  }
  subject: string
  ticker?: {
    value: string
    anSignatures: Signature[]
  }
  url?: {
    value: string
    anSignatures: Signature[]
  }
}
