import AssetFingerprint from '@emurgo/cip14-js'

export const assetFingerprint = (policyId: string, assetName?: string) =>
  new AssetFingerprint(
    Buffer.from(policyId, 'hex'),
    Buffer.from(assetName ?? '', 'hex')
  ).fingerprint()
