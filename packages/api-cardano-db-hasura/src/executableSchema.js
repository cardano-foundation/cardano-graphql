"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
exports.__esModule = true;
exports.buildSchema = exports.scalarResolvers = void 0;
var fs_1 = require("fs");
var apollo_server_1 = require("apollo-server");
var schema_1 = require("@graphql-tools/schema");
var delegate_1 = require("@graphql-tools/delegate");
var path_1 = require("path");
var util_1 = require("@cardano-graphql/util");
var graphql_scalars_1 = require("graphql-scalars");
var bignumber_js_1 = require("bignumber.js");
var queryComplexity_1 = require("./queryComplexity");
var GraphQLBigInt = require('graphql-bigint');
exports.scalarResolvers = {
    AssetFingerprint: util_1["default"].scalars.AssetFingerprint,
    BigInt: GraphQLBigInt,
    DateTime: util_1["default"].scalars.DateTimeUtcToIso,
    Hash28Hex: util_1["default"].scalars.Hash28Hex,
    Hash32Hex: util_1["default"].scalars.Hash32Hex,
    Hex: util_1["default"].scalars.Hex,
    IPv4: graphql_scalars_1.IPv4Resolver,
    IPv6: graphql_scalars_1.IPv6Resolver,
    JSON: graphql_scalars_1.JSONResolver,
    JSONObject: graphql_scalars_1.JSONObjectResolver,
    Lovelace: util_1["default"].scalars.Lovelace,
    Percentage: util_1["default"].scalars.Percentage,
    StakeAddress: util_1["default"].scalars.StakeAddress,
    StakePoolID: util_1["default"].scalars.StakePoolID,
    Timestamp: graphql_scalars_1.TimestampResolver,
    VRFVerificationKey: util_1["default"].scalars.VRFVerificationKey
};
function buildSchema(hasuraClient, genesis, cardanoNodeClient, customFieldsComplexity) {
    if (customFieldsComplexity === void 0) { customFieldsComplexity = queryComplexity_1.defaultComplexity; }
    return __awaiter(this, void 0, void 0, function () {
        var getComplexityExtension;
        var _this = this;
        return __generator(this, function (_a) {
            getComplexityExtension = function (operation, queryName) {
                if (operation in customFieldsComplexity) {
                    var operationMapping = customFieldsComplexity[operation];
                    if (queryName in operationMapping &&
                        operationMapping[queryName].extensions) {
                        // If it has a custom complexity then use that one and ignore the base cost,
                        // otherwise use the default with the base cost
                        return {
                            complexity: operationMapping[queryName].extensions.complexity ||
                                queryComplexity_1.getDefaultQueryComplexity(operationMapping[queryName].extensions.baseCost)
                        };
                    }
                }
                // If not found, then just return the default complexity estimators
                return { complexity: queryComplexity_1.getDefaultQueryComplexity() };
            };
            return [2 /*return*/, schema_1.makeExecutableSchema({
                    resolvers: Object.assign({}, exports.scalarResolvers, customFieldsComplexity, {
                        Mutation: {
                            submitTransaction: {
                                resolve: function (_root, args) { return __awaiter(_this, void 0, void 0, function () {
                                    var hash, error_1;
                                    return __generator(this, function (_a) {
                                        switch (_a.label) {
                                            case 0:
                                                _a.trys.push([0, 2, , 3]);
                                                return [4 /*yield*/, cardanoNodeClient.submitTransaction(args.transaction)];
                                            case 1:
                                                hash = _a.sent();
                                                return [2 /*return*/, { hash: hash }];
                                            case 2:
                                                error_1 = _a.sent();
                                                if (Array.isArray(error_1)) {
                                                    throw new apollo_server_1.ApolloError('Invalid Transaction', 'BAD_REQUEST', {
                                                        reasons: error_1.map(function (e) { return ({
                                                            name: e.name,
                                                            details: JSON.parse(e.message)
                                                        }); })
                                                    });
                                                }
                                                else {
                                                    throw new apollo_server_1.ApolloError(error_1);
                                                }
                                                return [3 /*break*/, 3];
                                            case 3: return [2 /*return*/];
                                        }
                                    });
                                }); },
                                selectionSet: null,
                                extensions: getComplexityExtension('Mutation', 'submitTransaction')
                            }
                        },
                        PaymentAddress: {
                            summary: {
                                resolve: function (parent, args) { return __awaiter(_this, void 0, void 0, function () {
                                    var error_2;
                                    return __generator(this, function (_a) {
                                        switch (_a.label) {
                                            case 0:
                                                _a.trys.push([0, 2, , 3]);
                                                return [4 /*yield*/, hasuraClient.getPaymentAddressSummary(parent.address, args.atBlock)];
                                            case 1: return [2 /*return*/, _a.sent()];
                                            case 2:
                                                error_2 = _a.sent();
                                                throw new apollo_server_1.ApolloError(error_2);
                                            case 3: return [2 /*return*/];
                                        }
                                    });
                                }); },
                                selectionSet: null,
                                extensions: getComplexityExtension('PaymentAddress', 'summary')
                            }
                        },
                        Query: {
                            activeStake: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'activeStake',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'activeStake')
                            },
                            activeStake_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'activeStake_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'activeStake_aggregate')
                            },
                            ada: {
                                resolve: function () { return __awaiter(_this, void 0, void 0, function () {
                                    var adaPots;
                                    return __generator(this, function (_a) {
                                        adaPots = hasuraClient.adaPotsToCalculateSupplyFetcher.value;
                                        if (adaPots === undefined || adaPots === null) {
                                            return [2 /*return*/, new apollo_server_1.ApolloError('ada query results are not ready yet. This can occur during startup.')];
                                        }
                                        return [2 /*return*/, {
                                                supply: {
                                                    circulating: adaPots.circulating,
                                                    max: genesis.shelley.maxLovelaceSupply,
                                                    total: new bignumber_js_1["default"](genesis.shelley.maxLovelaceSupply)
                                                        .minus(new bignumber_js_1["default"](adaPots.reserves))
                                                        .toString()
                                                }
                                            }];
                                    });
                                }); },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'ada')
                            },
                            assets: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'assets',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'assets')
                            },
                            assets_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'assets_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'assets_aggregate')
                            },
                            blocks: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'blocks',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'blocks')
                            },
                            blocks_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'blocks_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'blocks_aggregate')
                            },
                            cardano: {
                                resolve: function (_root, _args, context, info) { return __awaiter(_this, void 0, void 0, function () {
                                    var result, error_3;
                                    var _a;
                                    return __generator(this, function (_b) {
                                        switch (_b.label) {
                                            case 0:
                                                _b.trys.push([0, 2, , 3]);
                                                return [4 /*yield*/, delegate_1.delegateToSchema({
                                                        context: context,
                                                        fieldName: 'cardano',
                                                        info: info,
                                                        operation: 'query',
                                                        schema: hasuraClient.schema
                                                    })];
                                            case 1:
                                                result = _b.sent();
                                                if (((_a = result[0]) === null || _a === void 0 ? void 0 : _a.currentEpoch) === null) {
                                                    return [2 /*return*/, new apollo_server_1.ApolloError('currentEpoch is only available when close to the chain tip. This is expected during the initial chain-sync.')];
                                                }
                                                return [2 /*return*/, result[0]];
                                            case 2:
                                                error_3 = _b.sent();
                                                throw new apollo_server_1.ApolloError(error_3);
                                            case 3: return [2 /*return*/];
                                        }
                                    });
                                }); },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'cardano')
                            },
                            cardanoDbMeta: {
                                resolve: function () { return __awaiter(_this, void 0, void 0, function () {
                                    var slotNo, error_4;
                                    return __generator(this, function (_a) {
                                        switch (_a.label) {
                                            case 0:
                                                _a.trys.push([0, 2, , 3]);
                                                return [4 /*yield*/, cardanoNodeClient.getTipSlotNo()];
                                            case 1:
                                                slotNo = _a.sent();
                                                return [2 /*return*/, hasuraClient.getMeta(slotNo)];
                                            case 2:
                                                error_4 = _a.sent();
                                                throw new apollo_server_1.ApolloError(error_4.name === 'ModuleIsNotInitialized'
                                                    ? 'cardanoDbMeta query is not ready. Try again shortly'
                                                    : error_4.message);
                                            case 3: return [2 /*return*/];
                                        }
                                    });
                                }); },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'cardanoDbMeta')
                            },
                            collateralInputs: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'collateralInputs',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'collateralInputs')
                            },
                            collateralInputs_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'collateralInputs_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'collateralInputs_aggregate')
                            },
                            collateralOutputs: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'collateralOutputs',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'collateralOutputs')
                            },
                            collateralOutputs_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'collateralOutputs_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'collateralOutputs_aggregate')
                            },
                            delegations: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'delegations',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'delegations')
                            },
                            delegations_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'delegations_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'delegations_aggregate')
                            },
                            epochs: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'epochs',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'epochs')
                            },
                            epochs_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'epochs_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'epochs_aggregate')
                            },
                            genesis: {
                                resolve: function () { return __awaiter(_this, void 0, void 0, function () { return __generator(this, function (_a) {
                                    return [2 /*return*/, genesis];
                                }); }); },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'genesis')
                            },
                            paymentAddresses: {
                                resolve: function (_root, args) { return __awaiter(_this, void 0, void 0, function () {
                                    var _this = this;
                                    return __generator(this, function (_a) {
                                        return [2 /*return*/, args.addresses.map(function (address) { return __awaiter(_this, void 0, void 0, function () {
                                                return __generator(this, function (_a) {
                                                    return [2 /*return*/, { address: address }];
                                                });
                                            }); })];
                                    });
                                }); },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'paymentAddresses')
                            },
                            redeemers: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'redeemers',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'redeemers')
                            },
                            redeemers_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'redeemers_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'redeemers_aggregate')
                            },
                            rewards: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'rewards',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'rewards')
                            },
                            rewards_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'rewards_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'rewards_aggregate')
                            },
                            scripts: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'scripts',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'scripts')
                            },
                            scripts_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'scripts_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'scripts_aggregate')
                            },
                            stakeDeregistrations: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'stakeDeregistrations',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'stakeDeregistrations')
                            },
                            stakeDeregistrations_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'stakeDeregistrations_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'stakeDeregistrations_aggregate')
                            },
                            stakePools: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'stakePools',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'stakePools')
                            },
                            stakePools_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'stakePools_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'stakePools_aggregate')
                            },
                            stakeRegistrations: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'stakeRegistrations',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'stakeRegistrations')
                            },
                            stakeRegistrations_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'stakeRegistrations_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'stakeRegistrations_aggregate')
                            },
                            transactions: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'transactions',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'transactions')
                            },
                            transactions_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'transactions_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'transactions_aggregate')
                            },
                            tokenMints: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'tokenMints',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'tokenMints')
                            },
                            tokenMints_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'tokenMints_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'tokenMints_aggregate')
                            },
                            utxos: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'utxos',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'utxos')
                            },
                            utxos_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'utxos_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'utxos_aggregate')
                            },
                            withdrawals: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'withdrawals',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'withdrawals')
                            },
                            withdrawals_aggregate: {
                                resolve: function (_root, args, context, info) {
                                    return delegate_1.delegateToSchema({
                                        args: args,
                                        context: context,
                                        fieldName: 'withdrawals_aggregate',
                                        info: info,
                                        operation: 'query',
                                        schema: hasuraClient.schema
                                    });
                                },
                                selectionSet: null,
                                extensions: getComplexityExtension('Query', 'withdrawals_aggregate')
                            }
                        }
                    }),
                    typeDefs: fs_1["default"].readFileSync(path_1["default"].resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
                })];
        });
    });
}
exports.buildSchema = buildSchema;
