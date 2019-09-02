import gql from 'graphql-tag'

export const ledgerStats = gql`query {
    ledgerStats {
        blockHeight
    }
}`
