import {
  ComplexityEstimator,
  ComplexityEstimatorArgs
} from 'graphql-query-complexity'

type ComplexityExtension = {
  extensions: {
    complexity?: number | ComplexityEstimator;
    baseCost?: number;
  };
};

export type ComplexityMapping = { [key: string]: ComplexityExtension };

export type FieldsComplexityMapping = {
  [key: string]:
    | FieldsComplexityMapping
    | ComplexityMapping
    | ComplexityExtension;
};

const aggregateFieldsCosts = (
  pick: Array<'avg' | 'count' | 'max' | 'min' | 'sum'> = [],
  costs: {
    count?: number;
    avg?: number;
    max?: number;
    min?: number;
    sum?: number;
  } = {}
) => {
  const defaultCosts: { [key: string]: ComplexityExtension } = {
    avg: { extensions: { complexity: costs.avg || 2 } },
    count: { extensions: { complexity: costs.count || 1 } },
    max: { extensions: { complexity: costs.max || 2 } },
    min: { extensions: { complexity: costs.min || 2 } },
    sum: { extensions: { complexity: costs.sum || 2 } }
  }
  if (!pick || !pick.length) return defaultCosts

  const pickedFields: { [key: string]: ComplexityExtension } = {}
  pick.forEach((field) => {
    pickedFields[field] = defaultCosts[field]
  })
  return pickedFields
}

const booleanOperatorsCostMapping: { [key: string]: number } = {
  _eq: 1,
  _gt: 1,
  _gte: 1,
  _in: 1,
  _lt: 1,
  _lte: 1,
  _neq: 1,
  _nin: 1,
  _is_null: 1,
  _ilike: 2,
  _like: 2,
  _nilike: 2,
  _nlike: 2,
  _nsimilar: 2,
  _similar: 2
}

const calculateWhereComplexity = (where: any): number => {
  let cost: number = 0
  if (!where) return cost

  Object.entries(where).forEach(([key, value]: [string, any]) => {
    // Logical operators
    if (key === '_not') {
      cost += calculateWhereComplexity(value) || 0
      return
    }
    if (key === '_or' || key === '_and') {
      Object.values(value).forEach((condition) => {
        cost += calculateWhereComplexity(condition) || 0
      })
      return
    }

    // Simple filters
    const operators = Object.entries(value)
    if (
      operators.some(([operator, _]) => operator in booleanOperatorsCostMapping)
    ) {
      operators.forEach(([operator, v]) => {
        if (operator in booleanOperatorsCostMapping) {
          const operationCost = booleanOperatorsCostMapping[operator] || 0
          if (operator === '_in' || operator === '_nin') {
            if (Array.isArray(v)) {
              cost += operationCost * v.length
            }
          } else {
            cost += operationCost
          }
        }
      })
      return
    }

    // If none of the above we assume it's a nested filter
    cost += 1 + (calculateWhereComplexity(value) || 0)
  })
  return cost
}

const calculateOffsetComplexity = (offset: any): number => {
  if (!offset || isNaN(Number(offset))) return 0
  return Math.ceil(Number(offset) / 100)
}

const calculateOrderByComplexity = (orderBy: any[]): number => {
  const cost: number = 0
  if (!orderBy || !orderBy.length) return cost

  const calculateOrderByFieldComplexity = (field: {
    [key: string]: string;
  }): number => {
    return Object.entries(field).reduce(
      (finalCost, [_, value]: [string, any]) => {
        return typeof value === 'string'
          ? finalCost + 1
          : finalCost + 1 + (calculateOrderByFieldComplexity(value) || 0)
      },
      0
    )
  }
  return orderBy.reduce(
    (finalCost, field) =>
      finalCost + (calculateOrderByFieldComplexity(field) || 0),
    cost
  )
}

export const defaultComplexity: FieldsComplexityMapping = {
  Query: {
    activeStake_aggregate: { extensions: { baseCost: 2 } },
    assets_aggregate: { extensions: { baseCost: 2 } },
    blocks_aggregate: { extensions: { baseCost: 10 } },
    collateralInputs_aggregate: { extensions: { baseCost: 4 } },
    collateralOutputs_aggregate: { extensions: { baseCost: 4 } },
    delegations_aggregate: { extensions: { baseCost: 2 } },
    epochs_aggregate: { extensions: { baseCost: 2 } },
    redeemers_aggregate: { extensions: { baseCost: 2 } },
    rewards_aggregate: { extensions: { baseCost: 2 } },
    scripts_aggregate: { extensions: { baseCost: 2 } },
    stakeDeregistrations_aggregate: { extensions: { baseCost: 2 } },
    stakePools_aggregate: { extensions: { baseCost: 3 } },
    stakeRegistrations_aggregate: { extensions: { baseCost: 2 } },
    tokenMints_aggregate: { extensions: { baseCost: 2 } },
    transactions_aggregate: { extensions: { baseCost: 5 } },
    utxos_aggregate: { extensions: { baseCost: 4 } },
    withdrawals_aggregate: { extensions: { baseCost: 2 } }
  },
  ActiveStake_aggregate_fields: aggregateFieldsCosts(),
  Asset_aggregate_fields: aggregateFieldsCosts(['count']),
  Block_aggregate_fields: aggregateFieldsCosts(),
  CollateralInput_aggregate_fields: aggregateFieldsCosts(),
  CollateralOutput_aggregate_fields: aggregateFieldsCosts(),
  Delegation_aggregate_fields: aggregateFieldsCosts(['count']),
  Epoch_aggregate_fields: aggregateFieldsCosts(),
  Redeemer_aggregate_fields: aggregateFieldsCosts(),
  Reward_aggregate_fields: aggregateFieldsCosts(),
  Script_aggregate_fields: aggregateFieldsCosts(),
  StakeDeregistration_aggregate_fields: aggregateFieldsCosts(['count']),
  StakePool_aggregate_fields: aggregateFieldsCosts(),
  StakeRegistration_aggregate_fields: aggregateFieldsCosts(['count']),
  TokenMint_aggregate_fields: aggregateFieldsCosts(),
  Transaction_aggregate_fields: aggregateFieldsCosts(),
  TransactionInput_aggregate_fields: aggregateFieldsCosts(),
  TransactionOutput_aggregate_fields: aggregateFieldsCosts(),
  Withdrawal_aggregate_fields: aggregateFieldsCosts()
}

export const getDefaultQueryComplexity =
  (baseQueryCost: number = 1) =>
    ({ args, childComplexity }: ComplexityEstimatorArgs) => {
      const { where, offset, order_by: orderBy } = args

      const whereComplexity = calculateWhereComplexity(where) || 0
      const offsetComplexity = calculateOffsetComplexity(offset) + 1
      const orderByComplexity = calculateOrderByComplexity(orderBy) + 1

      // Base query cost shouldn't be less than 1
      const base = baseQueryCost < 1 ? 1 : baseQueryCost
      return (
        base *
      ((childComplexity + whereComplexity) *
        offsetComplexity *
        orderByComplexity)
      )
    }
