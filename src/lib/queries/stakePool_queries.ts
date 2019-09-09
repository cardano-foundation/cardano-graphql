import gql from 'graphql-tag'

export const stakePools = gql`query stakePools(
    $limit: PositiveInt,
    $order_by: [stakePools_order_by!],
    $offset: PositiveInt,
    $where: stakePools_filter
){
    stakePools(
        limit: $limit,
        order_by: $order_by,
        offset: $offset,
        where: $where
    ) {
        controlledStake
        id
        ticker
    }
}`
