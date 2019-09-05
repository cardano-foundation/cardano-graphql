import gql from 'graphql-tag'

export const ledger = gql`query {
    ledger {
        blockHeight
    }
}`
