"use strict";
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
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
var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
exports.__esModule = true;
exports.HasuraClient = void 0;
var util_1 = require("@cardano-graphql/util");
var cross_fetch_1 = require("cross-fetch");
var graphql_1 = require("graphql");
var graphql_request_1 = require("graphql-request");
var wrap_1 = require("@graphql-tools/wrap");
var p_retry_1 = require("p-retry");
var ts_log_1 = require("ts-log");
var bignumber_js_1 = require("bignumber.js");
var epochInformationNotYetAvailable = 'Epoch information not yet available. This is expected during the initial chain-sync.';
var HasuraClient = /** @class */ (function () {
    function HasuraClient(hasuraUri, pollingInterval, logger) {
        var _this = this;
        if (logger === void 0) { logger = ts_log_1.dummyLogger; }
        this.hasuraUri = hasuraUri;
        this.logger = logger;
        this.state = null;
        this.adaPotsToCalculateSupplyFetcher = new util_1.DataFetcher('AdaPotsToCalculateSupply', function () {
            try {
                return _this.getAdaPotsToCalculateSupply();
            }
            catch (error) {
                if (error.message !== epochInformationNotYetAvailable) {
                    console.debug(error.message);
                }
                _this.logger.trace({ err: error });
            }
        }, pollingInterval, this.logger);
        this.client = new graphql_request_1.GraphQLClient(this.hasuraUri + "/v1/graphql", {
            headers: {
                'X-Hasura-Role': 'cardano-graphql'
            }
        });
    }
    HasuraClient.prototype.getAdaPotsToCalculateSupply = function () {
        var _a, _b;
        return __awaiter(this, void 0, void 0, function () {
            var result, epochs, rewardsAggregate, utxosAggregate, withdrawalsAggregate, rewards, utxos, withdrawals, withdrawableRewards;
            return __generator(this, function (_c) {
                switch (_c.label) {
                    case 0: return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_1 || (templateObject_1 = __makeTemplateObject(["query {\n          epochs (limit: 1, order_by: { number: desc }) {\n              adaPots {\n                  reserves\n              }\n          }\n          rewards_aggregate {\n              aggregate {\n                  sum {\n                      amount\n                  }\n              }\n          }\n          utxos_aggregate {\n              aggregate {\n                  sum {\n                      value\n                  }\n              }\n          }\n          withdrawals_aggregate {\n              aggregate {\n                  sum {\n                      amount\n                  }\n              }\n          }\n      }"], ["query {\n          epochs (limit: 1, order_by: { number: desc }) {\n              adaPots {\n                  reserves\n              }\n          }\n          rewards_aggregate {\n              aggregate {\n                  sum {\n                      amount\n                  }\n              }\n          }\n          utxos_aggregate {\n              aggregate {\n                  sum {\n                      value\n                  }\n              }\n          }\n          withdrawals_aggregate {\n              aggregate {\n                  sum {\n                      amount\n                  }\n              }\n          }\n      }"]))))];
                    case 1:
                        result = _c.sent();
                        epochs = result.epochs, rewardsAggregate = result.rewards_aggregate, utxosAggregate = result.utxos_aggregate, withdrawalsAggregate = result.withdrawals_aggregate;
                        if (epochs.length === 0 || ((_a = epochs[0]) === null || _a === void 0 ? void 0 : _a.adaPots) === null) {
                            this.logger.debug({ module: 'HasuraClient' }, epochInformationNotYetAvailable);
                            throw new Error(epochInformationNotYetAvailable);
                        }
                        rewards = new bignumber_js_1["default"](rewardsAggregate.aggregate.sum.amount);
                        utxos = new bignumber_js_1["default"](utxosAggregate.aggregate.sum.value);
                        withdrawals = new bignumber_js_1["default"](withdrawalsAggregate.aggregate.sum.amount);
                        withdrawableRewards = rewards.minus(withdrawals);
                        return [2 /*return*/, {
                                circulating: utxos.plus(withdrawableRewards).toString(),
                                reserves: (_b = epochs[0]) === null || _b === void 0 ? void 0 : _b.adaPots.reserves
                            }];
                }
            });
        });
    };
    HasuraClient.prototype.initialize = function () {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== null)
                            return [2 /*return*/];
                        this.state = 'initializing';
                        this.logger.info({ module: 'HasuraClient' }, 'Initializing');
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                var _a;
                                return __generator(this, function (_b) {
                                    switch (_b.label) {
                                        case 0:
                                            _a = this;
                                            return [4 /*yield*/, this.buildHasuraSchema()];
                                        case 1:
                                            _a.schema = _b.sent();
                                            return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.75,
                                retries: 9,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Fetching Hasura schema via introspection', this.logger)
                            })];
                    case 1:
                        _a.sent();
                        this.logger.debug({ module: 'HasuraClient' }, 'graphql-engine setup');
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                return __generator(this, function (_a) {
                                    switch (_a.label) {
                                        case 0: return [4 /*yield*/, this.adaPotsToCalculateSupplyFetcher.initialize()];
                                        case 1:
                                            _a.sent();
                                            return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.1,
                                forever: true,
                                maxTimeout: 15000,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Initializing data fetchers', this.logger)
                            })];
                    case 2:
                        _a.sent();
                        this.logger.debug({ module: 'HasuraClient' }, 'Data fetchers initialized');
                        this.state = 'initialized';
                        this.logger.info({ module: 'HasuraClient' }, 'Initialized');
                        return [2 /*return*/];
                }
            });
        });
    };
    HasuraClient.prototype.shutdown = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.adaPotsToCalculateSupplyFetcher.shutdown()];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    HasuraClient.prototype.buildHasuraSchema = function () {
        return __awaiter(this, void 0, void 0, function () {
            var executor, coreTypes, schema, _a, _b, _i, coreTypes_1, t, gqlType;
            var _this = this;
            return __generator(this, function (_c) {
                switch (_c.label) {
                    case 0:
                        executor = function (_a) {
                            var document = _a.document, variables = _a.variables;
                            return __awaiter(_this, void 0, void 0, function () {
                                var query, fetchResult, error_1;
                                return __generator(this, function (_b) {
                                    switch (_b.label) {
                                        case 0:
                                            query = graphql_1.print(document);
                                            _b.label = 1;
                                        case 1:
                                            _b.trys.push([1, 3, , 4]);
                                            return [4 /*yield*/, cross_fetch_1["default"](this.hasuraUri + "/v1/graphql", {
                                                    method: 'POST',
                                                    headers: {
                                                        'Content-Type': 'application/json',
                                                        'X-Hasura-Role': 'cardano-graphql'
                                                    },
                                                    body: JSON.stringify({ query: query, variables: variables })
                                                })];
                                        case 2:
                                            fetchResult = _b.sent();
                                            return [2 /*return*/, fetchResult.json()];
                                        case 3:
                                            error_1 = _b.sent();
                                            this.logger.error({ err: error_1 });
                                            throw error_1;
                                        case 4: return [2 /*return*/];
                                    }
                                });
                            });
                        };
                        coreTypes = [
                            'Block',
                            'Cardano',
                            'Epoch',
                            'Block',
                            'Transaction'
                        ];
                        _a = wrap_1.wrapSchema;
                        _b = {};
                        return [4 /*yield*/, wrap_1.introspectSchema(executor)];
                    case 1:
                        schema = _a.apply(void 0, [(_b.schema = _c.sent(),
                                _b.executor = executor,
                                _b)]);
                        for (_i = 0, coreTypes_1 = coreTypes; _i < coreTypes_1.length; _i++) {
                            t = coreTypes_1[_i];
                            gqlType = schema.getType(t);
                            if (!gqlType) {
                                throw new Error("Remote schema is missing " + t);
                            }
                        }
                        return [2 /*return*/, schema];
                }
            });
        });
    };
    HasuraClient.prototype.getCurrentProtocolVersion = function () {
        var _a;
        return __awaiter(this, void 0, void 0, function () {
            var result;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0: return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_2 || (templateObject_2 = __makeTemplateObject(["query {\n          epochs (limit: 1, order_by: { number: desc }) {\n              protocolParams {\n                  protocolVersion\n              }\n          }\n      }"], ["query {\n          epochs (limit: 1, order_by: { number: desc }) {\n              protocolParams {\n                  protocolVersion\n              }\n          }\n      }"]))))];
                    case 1:
                        result = _b.sent();
                        return [2 /*return*/, (_a = result.epochs[0]) === null || _a === void 0 ? void 0 : _a.protocolParams.protocolVersion];
                }
            });
        });
    };
    HasuraClient.prototype.getPaymentAddressSummary = function (address, atBlock) {
        return __awaiter(this, void 0, void 0, function () {
            var args, query, result, map, _i, _a, utxo, current, _b, _c, token, current;
            return __generator(this, function (_d) {
                switch (_d.label) {
                    case 0:
                        args = 'address: { _eq: $address }';
                        if (atBlock) {
                            args = args + '\n transaction: { block: { number: { _lte: $atBlock }}}';
                        }
                        query = "query PaymentAddressSummary (\n          $address: String!\n          $atBlock: Int\n      ){\n          utxos (\n              where: {\n                  _and: {\n                      " + args + "\n                  }\n              }\n          ) {\n              value\n              tokens {\n                  asset {\n                      assetId\n                      assetName\n                      decimals\n                      description\n                      fingerprint\n                      logo\n                      metadataHash\n                      name\n                      ticker\n                      tokenMints {\n                          quantity\n                          transaction {\n                              hash\n                          }\n                      }\n                      tokenMints_aggregate {\n                          aggregate {\n                              count\n                              max {\n                                  quantity\n                              }\n                              min {\n                                  quantity\n                              }\n                              sum {\n                                  quantity\n                              }\n                          }\n                      }\n                      url\n                      policyId\n                  }\n                  quantity\n              }\n          }\n          utxos_aggregate (\n              where: {\n                  _and: {\n                      " + args + "\n                  }\n              }\n          ) {\n              aggregate {\n                  count\n              }\n          }\n      }";
                        return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_3 || (templateObject_3 = __makeTemplateObject(["", ""], ["", ""])), query), { address: address, atBlock: atBlock })];
                    case 1:
                        result = _d.sent();
                        map = new Map();
                        for (_i = 0, _a = result.utxos; _i < _a.length; _i++) {
                            utxo = _a[_i];
                            if (map.has('ada')) {
                                current = map.get('ada');
                                map.set('ada', __assign(__assign({}, current), {
                                    quantity: new bignumber_js_1["default"](current.quantity)
                                        .plus(new bignumber_js_1["default"](utxo.value))
                                        .toString()
                                }));
                            }
                            else {
                                map.set('ada', {
                                    asset: {
                                        assetId: '\\xada',
                                        assetName: '\\xada',
                                        name: 'ada',
                                        policyId: '\\xada',
                                        tokenMints: [],
                                        tokenMints_aggregate: {
                                            aggregate: {
                                                count: 'na',
                                                max: {
                                                    quantity: 'na'
                                                },
                                                min: {
                                                    quantity: 'na'
                                                },
                                                sum: {
                                                    quantity: 'na'
                                                }
                                            },
                                            nodes: []
                                        }
                                    },
                                    quantity: utxo.value
                                });
                            }
                            for (_b = 0, _c = utxo.tokens; _b < _c.length; _b++) {
                                token = _c[_b];
                                if (map.has(token.asset.assetId)) {
                                    current = map.get(token.asset.assetId);
                                    map.set(token.asset.assetId, __assign(__assign({}, current), {
                                        quantity: new bignumber_js_1["default"](current.quantity)
                                            .plus(new bignumber_js_1["default"](token.quantity))
                                            .toString()
                                    }));
                                }
                                else {
                                    map.set(token.asset.assetId, token);
                                }
                            }
                        }
                        return [2 /*return*/, {
                                assetBalances: __spreadArrays(map.values()),
                                utxosCount: result.utxos_aggregate.aggregate.count
                            }];
                }
            });
        });
    };
    HasuraClient.prototype.getMeta = function (nodeTipSlotNumber) {
        var _a;
        return __awaiter(this, void 0, void 0, function () {
            var result, tip, lastEpoch, syncPercentage;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0: return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_4 || (templateObject_4 = __makeTemplateObject(["query {\n          epochs (limit: 1, order_by: { number: desc }) {\n              number\n          }\n          cardano {\n              tip {\n                  epoch {\n                      number\n                  }\n                  slotNo\n                  forgedAt\n              }\n          }}"], ["query {\n          epochs (limit: 1, order_by: { number: desc }) {\n              number\n          }\n          cardano {\n              tip {\n                  epoch {\n                      number\n                  }\n                  slotNo\n                  forgedAt\n              }\n          }}"]))))];
                    case 1:
                        result = _b.sent();
                        tip = (result === null || result === void 0 ? void 0 : result.cardano[0]).tip;
                        lastEpoch = result === null || result === void 0 ? void 0 : result.epochs[0];
                        syncPercentage = tip.slotNo / nodeTipSlotNumber * 100;
                        return [2 /*return*/, {
                                // cardano-db-sync writes the epoch record at the end of each epoch during times of bulk sync
                                // The initialization state can be determined by comparing the last epoch record against the
                                // tip
                                initialized: lastEpoch.number === ((_a = tip.epoch) === null || _a === void 0 ? void 0 : _a.number),
                                // we cannot assume that actual db-sync syncPercentage will be less or equal to node sync state due to race condition at the query time
                                syncPercentage: syncPercentage > 100 ? 100 : syncPercentage
                            }];
                }
            });
        });
    };
    return HasuraClient;
}());
exports.HasuraClient = HasuraClient;
var templateObject_1, templateObject_2, templateObject_3, templateObject_4;
