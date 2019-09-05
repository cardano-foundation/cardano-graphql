import gql from 'graphql-tag'

export const transactions = gql`query transactions(
    $limit: Int,
    $order_by: [transactions_order_by!],
    $offset: Int,
    $where: transactions_filter
){
    transactions(
        limit: $limit,
        order_by: $order_by,
        offset: $offset,
        where: $where
    ) {
        block {
            number
        }
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
