import gql from 'graphql-tag'

export const utxoSet = gql`query utxoSet(
    $limit: Int,
    $order_by: [utxoSet_order_by!],
    $offset: Int,
    $where: utxoSet_filter
){
    utxoSet(
        limit: $limit,
        order_by: $order_by,
        offset: $offset,
        where: $where
    ) {
        address
        value
    }
}`
