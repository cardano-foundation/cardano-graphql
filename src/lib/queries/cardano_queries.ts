import gql from 'graphql-tag'

export const cardano = gql`query {
    cardano {
        blockHeight
        configuration {
            fees {
                base
                coefficient
            }
        }
        currentEpoch {
            number
        }
        latestBlock {
            number
        }
        stakeDistribution {
            controlledStake
            createdAt
            description
            id
            isCharity
            ownStake
            performance
            profitMargin
            name
            ranking
            retirement
            retiring
            rewardPayouts {
                slot {
                    number
                }
                amount
            }
            ticker
            url
        }
    }
}`
