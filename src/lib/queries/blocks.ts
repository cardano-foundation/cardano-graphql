import gql from 'graphql-tag'

export const blocksWithNoTx = gql`query blocksWithNoTx($filter: BlockFilter){
    blocks(filter: $filter) {
        id
        merkelRootHash
        number
        previousBlockNo
        size
        slotNo
    }
}`

export const blocksWithSomeTx = gql`query blocksWithSomeTxs($filter: BlockFilter, $txLimit: Int){
    blocks(filter: $filter) {
        id
        merkelRootHash
        number
        previousBlockNo
        size
        slotNo
        transactions(limit: $txLimit) {
            id
            fee
            inputs {
                sourceTxId
                sourceTxIndex
                address
                value
            }
            outputs {
                value
                address
            }
        }
    }
}`

export const nestedBlocks = gql`query nestedBlocks($filter: BlockFilter){
    blocks(filter: $filter) {
        id
        previousBlock {
            id
            previousBlock {
                id
                previousBlock {
                    id
                }
            }
        }
    }
}`
