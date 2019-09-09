import gql from 'graphql-tag'

export const blocksWithNoTx = gql`query blocksWithNoTx(
    $limit: PositiveInt,
    $order_by: [blocks_order_by!],
    $offset: PositiveInt,
    $where: blocks_filter
){
    blocks(
        limit: $limit,
        order_by: $order_by,
        offset: $offset,
        where: $where
    ) {
        id
        merkelRootHash
        number
        previousBlock {
            number
        }
        size
        slot {
            number
        }
    }
}`

export const blocksWithTxs = gql`query blocksWithSomeTxs(
    $limit: PositiveInt,
    $order_by: [blocks_order_by!],
    $offset: PositiveInt,
    $where: blocks_filter
){
    blocks(
        limit: $limit,
        order_by: $order_by,
        offset: $offset,
        where: $where
    ) {
        id
        merkelRootHash
        number
        previousBlock {
            number
        }
        size
        slot {
            number
        }
        transactions {
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
                value {
                    currency
                    amount
                }
                address
            }
        }
    }
}`

export const nestedBlocks = gql`query nestedBlocks(
    $limit: PositiveInt,
    $order_by: [blocks_order_by!],
    $where: blocks_filter
){
    blocks(
        limit: $limit,
        order_by: $order_by,
        where: $where
    ) {
        id
        previousBlock {
            number
            previousBlock {
                number
                previousBlock {
                    number
                }
            }
        }
    }
}`
