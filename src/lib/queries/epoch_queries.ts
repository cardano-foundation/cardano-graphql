import gql from 'graphql-tag'

export const epochDetails = gql`query epochDetails(
    $limit: Int,
    $order_by: [epochs_order_by!],
    $offset: Int,
    $where: epochs_filter
){
    epochs(
        limit: $limit,
        order_by: $order_by,
        offset: $offset,
        where: $where
    ) {
        blocks {
            epoch {
                number
            }
            fees
            id
            merkelRootHash
            number
            previousBlock {
                number
            }
            size
            slot {
                epoch {
                    number
                }
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
                    address
                    value {
                        currency
                        amount
                    }
                }
            }
        }
        endedAt
        output {
            currency
            amount
        }
        number
        slots {
            block {
                number
            }
            epoch {
                number
            }
            number
        }
        startedAt
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
                address
                value {
                    currency
                    amount
                }
            }
        }
        transactionsCount
    }
}`
