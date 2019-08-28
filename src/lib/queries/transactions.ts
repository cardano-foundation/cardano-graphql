import gql from 'graphql-tag'

export const transactions = gql`query transactions($filter: TransactionFilter!, $first: Int){
    transactions(filter: $filter, first: $first) {
        blockNo
        fee
        id
        inputs {
            address
            value
        }
        outputs {
            value
            address
        }
    }
}`
