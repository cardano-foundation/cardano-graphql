import gql from 'graphql-tag'

export const transactions = gql`query transactions(
    $limit: PositiveInt,
    $order_by: [transactions_order_by!],
    $offset: PositiveInt,
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
            value {
                currency
                amount
            }
        }
        outputs {
            address
            value {
                currency
                amount
            }
        }
    }
}`
