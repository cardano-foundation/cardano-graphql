import gql from 'graphql-tag'

export const ledgerMeta = gql`query {
    ledger {
        blockHeight
    }
}`
