import { buildSchema, parse, print } from 'graphql'
import { NestedArgumentsTransform } from '@src/NestedArgumentsTransform'

const targetSchema = buildSchema(`
  scalar bytea
  input bytea_comparison_exp { _eq: bytea _in: [bytea!] }
  input String_comparison_exp { _eq: String }
  input TransactionOutput_bool_exp {
    paymentCredential: bytea_comparison_exp
    stakeAddress: String_comparison_exp
  }
  input Transaction_bool_exp {
    hash: bytea_comparison_exp
    outputs: TransactionOutput_bool_exp
  }
  type TransactionOutput { index: Int }
  type Transaction {
    hash: bytea
    outputs(where: TransactionOutput_bool_exp): [TransactionOutput!]!
  }
  type Query { transactions(where: Transaction_bool_exp): [Transaction!]! }
`)

const paymentCredential = 'a48744c1584c58c2995cba1fa26b37f3999ee8cedac0ef241662f53d'
const stakeAddress = 'stake_test1urr29x8z8yp6tmhqe22v0865md9kgnz5suszum5mrj66luq23w8d7'

const transform = (query: string, variables: Record<string, any> = {}) =>
  new NestedArgumentsTransform().transformRequest(
    { document: parse(query), variables },
    { targetSchema }
  )

describe('NestedArgumentsTransform', () => {
  it('prefixes nested bytea literals and leaves text literals untouched', () => {
    const result = transform(`{
      transactions {
        outputs(where: { paymentCredential: { _eq: "${paymentCredential}" }, stakeAddress: { _eq: "${stakeAddress}" } }) { index }
      }
    }`)
    const printed = print(result.document)
    expect(printed).toContain(`_eq: "\\\\x${paymentCredential}"`)
    expect(printed).toContain(`_eq: "${stakeAddress}"`)
  })

  it('prefixes every value of a nested bytea list literal', () => {
    const result = transform(`{
      transactions {
        outputs(where: { paymentCredential: { _in: ["${paymentCredential}", "\\\\x${paymentCredential}"] } }) { index }
      }
    }`)
    expect(print(result.document)).toContain(`_in: ["\\\\x${paymentCredential}", "\\\\x${paymentCredential}"]`)
  })

  it('retypes nested custom scalar variables to the target types and prefixes bytea values', () => {
    const result = transform(`query ($p: Hash28Hex!, $s: StakeAddress!) {
      transactions {
        outputs(where: { paymentCredential: { _eq: $p }, stakeAddress: { _eq: $s } }) { index }
      }
    }`, { p: paymentCredential, s: stakeAddress })
    const printed = print(result.document)
    expect(printed).toContain('$p: bytea!')
    expect(printed).toContain('$s: String!')
    expect(result.variables).toEqual({ p: `\\x${paymentCredential}`, s: stakeAddress })
  })

  it('prefixes bytea fields inside a whole input object variable', () => {
    const result = transform(`query ($where: TransactionOutput_bool_exp) {
      transactions { outputs(where: $where) { index } }
    }`, { where: { paymentCredential: { _in: [paymentCredential] }, stakeAddress: { _eq: stakeAddress } } })
    expect(print(result.document)).toContain('$where: TransactionOutput_bool_exp')
    expect(result.variables.where).toEqual({
      paymentCredential: { _in: [`\\x${paymentCredential}`] },
      stakeAddress: { _eq: stakeAddress }
    })
  })

  it('does not double prefix values that already carry the bytea prefix', () => {
    const result = transform(`query ($p: Hash28Hex!) {
      transactions { outputs(where: { paymentCredential: { _eq: $p } }) { index } }
    }`, { p: `\\x${paymentCredential}` })
    expect(result.variables.p).toBe(`\\x${paymentCredential}`)
  })

  it('returns the request unchanged without a target schema', () => {
    const request = { document: parse('{ transactions { hash } }'), variables: {} }
    expect(new NestedArgumentsTransform().transformRequest(request)).toBe(request)
  })
})
