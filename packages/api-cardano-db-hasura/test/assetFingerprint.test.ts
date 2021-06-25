import { assetFingerprint } from '@src/assetFingerprint'

// https://github.com/cardano-foundation/CIPs/blob/master/CIP-0014/CIP-0014.md

describe('assetFingerprint', () => {
  it('produces a result to match the CIP test vectors', async () => {
    expect(
      assetFingerprint(
        '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373')
    ).toEqual('asset1rjklcrnsdzqp65wjgrg55sy9723kw09mlgvlc3')
    expect(
      assetFingerprint(
        '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc37e')
    ).toEqual('asset1nl0puwxmhas8fawxp8nx4e2q3wekg969n2auw3')
    expect(
      assetFingerprint(
        '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209')
    ).toEqual('asset1uyuxku60yqe57nusqzjx38aan3f2wq6s93f6ea')
    expect(
      assetFingerprint(
        '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
        '504154415445')
    ).toEqual('asset13n25uv0yaf5kus35fm2k86cqy60z58d9xmde92')
    expect(
      assetFingerprint(
        '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209',
        '504154415445')
    ).toEqual('asset1hv4p5tv2a837mzqrst04d0dcptdjmluqvdx9k3')
    expect(
      assetFingerprint(
        '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209',
        '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373')
    ).toEqual('asset1aqrdypg669jgazruv5ah07nuyqe0wxjhe2el6f')
    expect(
      assetFingerprint(
        '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
        '1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209')
    ).toEqual('asset17jd78wukhtrnmjh3fngzasxm8rck0l2r4hhyyt')
    expect(
      assetFingerprint(
        '7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373',
        '0000000000000000000000000000000000000000000000000000000000000000'
      )
    ).toEqual('asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w')
  })
})
