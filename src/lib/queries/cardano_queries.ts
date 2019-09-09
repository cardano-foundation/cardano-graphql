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
            pledgedStake
            performance
            profitMargin
            name
            ranking
            retirementEpochNumber
            slotsElected {
                number
                rewardPayouts {
                    address
                    amount
                }
            }
            ticker
            url
        }
    }
}`
