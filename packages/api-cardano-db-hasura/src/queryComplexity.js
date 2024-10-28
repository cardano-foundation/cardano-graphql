"use strict";
exports.__esModule = true;
exports.getDefaultQueryComplexity = exports.defaultComplexity = void 0;
var aggregateFieldsCosts = function (pick, costs) {
    if (pick === void 0) { pick = []; }
    if (costs === void 0) { costs = {}; }
    var defaultCosts = {
        count: { extensions: { complexity: costs.count || 1 } },
        max: { extensions: { complexity: costs.max || 2 } },
        min: { extensions: { complexity: costs.min || 2 } },
        sum: { extensions: { complexity: costs.sum || 2 } }
    };
    if (!pick || !pick.length)
        return defaultCosts;
    var pickedFields = {};
    pick.forEach(function (field) {
        pickedFields[field] = defaultCosts[field];
    });
    return pickedFields;
};
var booleanOperatorsCostMapping = {
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
};
var calculateWhereComplexity = function (where) {
    var cost = 0;
    if (!where)
        return cost;
    Object.entries(where).forEach(function (_a) {
        var key = _a[0], value = _a[1];
        // Logical operators
        if (key === '_not') {
            cost += calculateWhereComplexity(value) || 0;
            return;
        }
        if (key === '_or' || key === '_and') {
            Object.values(value).forEach(function (condition) {
                cost += calculateWhereComplexity(condition) || 0;
            });
            return;
        }
        // Simple filters
        var operators = Object.entries(value);
        if (operators.some(function (_a) {
            var operator = _a[0], _ = _a[1];
            return operator in booleanOperatorsCostMapping;
        })) {
            operators.forEach(function (_a) {
                var operator = _a[0], v = _a[1];
                if (operator in booleanOperatorsCostMapping) {
                    var operationCost = booleanOperatorsCostMapping[operator] || 0;
                    if (operator === '_in' || operator === '_nin') {
                        if (Array.isArray(v)) {
                            cost += operationCost * v.length;
                        }
                    }
                    else {
                        cost += operationCost;
                    }
                }
            });
            return;
        }
        // If none of the above we assume it's a nested filter
        cost += 1 + (calculateWhereComplexity(value) || 0);
    });
    return cost;
};
var calculateOffsetComplexity = function (offset) {
    if (!offset || isNaN(Number(offset)))
        return 0;
    return Math.ceil(Number(offset) / 100);
};
var calculateOrderByComplexity = function (orderBy) {
    var cost = 0;
    if (!orderBy || !orderBy.length)
        return cost;
    var calculateOrderByFieldComplexity = function (field) {
        return Object.entries(field).reduce(function (finalCost, _a) {
            var _ = _a[0], value = _a[1];
            return typeof value === 'string'
                ? finalCost + 1
                : finalCost + 1 + (calculateOrderByFieldComplexity(value) || 0);
        }, 0);
    };
    return orderBy.reduce(function (finalCost, field) {
        return finalCost + (calculateOrderByFieldComplexity(field) || 0);
    }, cost);
};
exports.defaultComplexity = {
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
};
exports.getDefaultQueryComplexity = function (baseQueryCost) {
    if (baseQueryCost === void 0) { baseQueryCost = 1; }
    return function (_a) {
        var args = _a.args, childComplexity = _a.childComplexity;
        var where = args.where, offset = args.offset, orderBy = args.order_by;
        var whereComplexity = calculateWhereComplexity(where) || 0;
        var offsetComplexity = calculateOffsetComplexity(offset) + 1;
        var orderByComplexity = calculateOrderByComplexity(orderBy) + 1;
        // Base query cost shouldn't be less than 1
        var base = baseQueryCost < 1 ? 1 : baseQueryCost;
        return (base *
            ((childComplexity + whereComplexity) *
                offsetComplexity *
                orderByComplexity));
    };
};
