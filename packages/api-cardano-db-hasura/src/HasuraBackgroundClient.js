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
exports.__esModule = true;
exports.HasuraBackgroundClient = void 0;
var child_process_1 = require("child_process");
var util_1 = require("@cardano-graphql/util");
var graphql_request_1 = require("graphql-request");
var p_retry_1 = require("p-retry");
var path_1 = require("path");
var ts_log_1 = require("ts-log");
var epochInformationNotYetAvailable = 'Epoch information not yet available. This is expected during the initial chain-sync.';
var withHexPrefix = function (value) { return "\\x" + (value !== undefined ? value : ''); };
var HasuraBackgroundClient = /** @class */ (function () {
    function HasuraBackgroundClient(hasuraCliPath, hasuraCliExtPath, hasuraUri, logger) {
        if (logger === void 0) { logger = ts_log_1.dummyLogger; }
        this.hasuraCliPath = hasuraCliPath;
        this.hasuraCliExtPath = hasuraCliExtPath;
        this.hasuraUri = hasuraUri;
        this.logger = logger;
        this.state = null;
        this.applyingSchemaAndMetadata = false;
        this.client = new graphql_request_1.GraphQLClient(this.hasuraUri + "/v1/graphql", {
            headers: {
                'X-Hasura-Role': 'cardano-graphql'
            }
        });
    }
    HasuraBackgroundClient.prototype.hasuraCli = function (command) {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                return [2 /*return*/, new Promise(function (resolve, reject) {
                        child_process_1.exec(_this.hasuraCliPath + " --cli-ext-path " + _this.hasuraCliExtPath + " --skip-update-check --project " + path_1["default"].resolve(__dirname, '..', 'hasura', 'project') + " --endpoint " + _this.hasuraUri + " " + command, function (error, stdout) {
                            if (error) {
                                reject(error);
                            }
                            if (stdout !== '')
                                _this.logger.debug({ module: 'HasuraBackgroundClient' }, stdout);
                            resolve({ module: 'HasuraBackgroundClient' });
                        });
                    })];
            });
        });
    };
    HasuraBackgroundClient.prototype.initialize = function () {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== null)
                            return [2 /*return*/];
                        this.state = 'initializing';
                        this.logger.info({ module: 'HasuraBackgroundClient' }, 'Initializing');
                        return [4 /*yield*/, this.applySchemaAndMetadata()];
                    case 1:
                        _a.sent();
                        this.logger.debug({ module: 'HasuraBackgroundClient' }, 'graphql-engine setup');
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                var result;
                                return __generator(this, function (_a) {
                                    switch (_a.label) {
                                        case 0: return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_1 || (templateObject_1 = __makeTemplateObject(["query {\n            epochs (limit: 1, order_by: { number: desc }) {\n                number\n            }\n        }"], ["query {\n            epochs (limit: 1, order_by: { number: desc }) {\n                number\n            }\n        }"]))))];
                                        case 1:
                                            result = _a.sent();
                                            if (result.epochs.length === 0) {
                                                this.logger.debug({ module: 'HasuraBackgroundClient' }, epochInformationNotYetAvailable);
                                                throw new Error(epochInformationNotYetAvailable);
                                            }
                                            return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.05,
                                retries: 10,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Detecting DB sync state has reached minimum progress', this.logger)
                            })];
                    case 2:
                        _a.sent();
                        this.logger.debug({ module: 'HasuraBackgroundClient' }, 'DB sync state has reached minimum progress');
                        this.state = 'initialized';
                        this.logger.info({ module: 'HasuraBackgroundClient' }, 'Initialized');
                        return [2 /*return*/];
                }
            });
        });
    };
    HasuraBackgroundClient.prototype.shutdown = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                this.state = null;
                return [2 /*return*/];
            });
        });
    };
    HasuraBackgroundClient.prototype.applySchemaAndMetadata = function () {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.applyingSchemaAndMetadata)
                            return [2 /*return*/];
                        this.applyingSchemaAndMetadata = true;
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                return __generator(this, function (_a) {
                                    switch (_a.label) {
                                        case 0: return [4 /*yield*/, this.hasuraCli('migrate --database-name default apply --down all')];
                                        case 1:
                                            _a.sent();
                                            return [4 /*yield*/, this.hasuraCli('migrate --database-name default apply --up all')];
                                        case 2:
                                            _a.sent();
                                            return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.75,
                                retries: 9,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Applying PostgreSQL schema migrations', this.logger)
                            })];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                return __generator(this, function (_a) {
                                    switch (_a.label) {
                                        case 0: return [4 /*yield*/, this.hasuraCli('metadata clear')];
                                        case 1:
                                            _a.sent();
                                            return [4 /*yield*/, this.hasuraCli('metadata apply')];
                                        case 2:
                                            _a.sent();
                                            return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.75,
                                retries: 9,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Applying Hasura metadata', this.logger)
                            })];
                    case 2:
                        _a.sent();
                        this.applyingSchemaAndMetadata = false;
                        return [2 /*return*/];
                }
            });
        });
    };
    HasuraBackgroundClient.prototype.deleteAssetsAfterSlot = function (slotNo) {
        return __awaiter(this, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.logger.debug({ module: 'HasuraClient', slotNo: slotNo }, 'deleting assets found in tokens after slot');
                        return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_2 || (templateObject_2 = __makeTemplateObject(["mutation DeleteAssetsAfterSlot($slotNo: Int!) {\n          delete_assets(\n              where: {\n                  firstAppearedInSlot: {\n                      _gt: $slotNo\n                  }\n              }\n          ) {\n              affected_rows\n          }\n      }"], ["mutation DeleteAssetsAfterSlot($slotNo: Int!) {\n          delete_assets(\n              where: {\n                  firstAppearedInSlot: {\n                      _gt: $slotNo\n                  }\n              }\n          ) {\n              affected_rows\n          }\n      }"]))), {
                                slotNo: slotNo
                            })];
                    case 1:
                        result = _a.sent();
                        return [2 /*return*/, result.delete_assets.affected_rows];
                }
            });
        });
    };
    HasuraBackgroundClient.prototype.hasAsset = function (assetId) {
        return __awaiter(this, void 0, void 0, function () {
            var result, response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_3 || (templateObject_3 = __makeTemplateObject(["query HasAsset (\n          $assetId: bytea!\n      ) {\n          assets (\n              where: { assetId: { _eq: $assetId }}\n          ) {\n              assetId\n          }\n      }"], ["query HasAsset (\n          $assetId: bytea!\n      ) {\n          assets (\n              where: { assetId: { _eq: $assetId }}\n          ) {\n              assetId\n          }\n      }"]))), {
                            assetId: withHexPrefix(assetId)
                        })];
                    case 1:
                        result = _a.sent();
                        response = result.assets.length > 0;
                        this.logger.debug({ module: 'HasuraClient', assetId: assetId, hasAsset: response }, 'Has asset?');
                        return [2 /*return*/, response];
                }
            });
        });
    };
    HasuraBackgroundClient.prototype.getMostRecentPointWithNewAsset = function () {
        return __awaiter(this, void 0, void 0, function () {
            var point;
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: 
                    // Handles possible race condition between the internal chain-follower, which manages the Asset table,
                    // and cardano-db-sync's which managed the block table.
                    return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                            var result, _a, hash, slotNo;
                            return __generator(this, function (_b) {
                                switch (_b.label) {
                                    case 0: return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_4 || (templateObject_4 = __makeTemplateObject(["query {\n            assets (\n                limit: 1\n                offset: 1\n                order_by: { firstAppearedInBlock: { slotNo: desc }}\n            ) {\n                firstAppearedInBlock {\n                    hash\n                    slotNo\n                }\n            }\n        }"], ["query {\n            assets (\n                limit: 1\n                offset: 1\n                order_by: { firstAppearedInBlock: { slotNo: desc }}\n            ) {\n                firstAppearedInBlock {\n                    hash\n                    slotNo\n                }\n            }\n        }"]))))];
                                    case 1:
                                        result = _b.sent();
                                        if (result.errors !== undefined) {
                                            throw new Error(result.errors);
                                        }
                                        if (result.assets.length !== 0) {
                                            if (result.assets[0].firstAppearedInBlock === null) {
                                                throw new Error('cardano-db-sync is lagging behind the asset sync operation.');
                                            }
                                            _a = result.assets[0].firstAppearedInBlock, hash = _a.hash, slotNo = _a.slotNo;
                                            point = {
                                                slot: Number(slotNo),
                                                id: hash.substring(2)
                                            };
                                        }
                                        else {
                                            point = null;
                                        }
                                        return [2 /*return*/];
                                }
                            });
                        }); }, {
                            factor: 1.5,
                            retries: 1000,
                            onFailedAttempt: util_1["default"].onFailedAttemptFor('Getting the most recent point with a new asset', this.logger)
                        })];
                    case 1:
                        // Handles possible race condition between the internal chain-follower, which manages the Asset table,
                        // and cardano-db-sync's which managed the block table.
                        _a.sent();
                        return [2 /*return*/, point];
                }
            });
        });
    };
    HasuraBackgroundClient.prototype.addAssetMetadata = function (asset) {
        return __awaiter(this, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.logger.info({ module: 'HasuraClient', assetId: asset.assetId }, 'Adding metadata to asset');
                        return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_5 || (templateObject_5 = __makeTemplateObject(["mutation AddAssetMetadata(\n          $assetId: bytea!\n          $decimals: Int\n          $description: String\n          $logo: String\n          $metadataHash: bpchar!\n          $name: String\n          $ticker: String\n          $url: String\n      ) {\n          update_assets(\n              where: {\n                  assetId: { _eq: $assetId }\n              },\n              _set: {\n                  decimals: $decimals\n                  description: $description\n                  logo: $logo\n                  metadataHash: $metadataHash\n                  name: $name\n                  ticker: $ticker\n                  url: $url\n              }\n          ) {\n              affected_rows\n              returning {\n                  assetId\n              }\n          }\n      }"], ["mutation AddAssetMetadata(\n          $assetId: bytea!\n          $decimals: Int\n          $description: String\n          $logo: String\n          $metadataHash: bpchar!\n          $name: String\n          $ticker: String\n          $url: String\n      ) {\n          update_assets(\n              where: {\n                  assetId: { _eq: $assetId }\n              },\n              _set: {\n                  decimals: $decimals\n                  description: $description\n                  logo: $logo\n                  metadataHash: $metadataHash\n                  name: $name\n                  ticker: $ticker\n                  url: $url\n              }\n          ) {\n              affected_rows\n              returning {\n                  assetId\n              }\n          }\n      }"]))), __assign(__assign({}, asset), { assetId: withHexPrefix(asset.assetId) }))];
                    case 1:
                        result = _a.sent();
                        if (result.errors !== undefined) {
                            throw new Error(result.errors);
                        }
                        return [2 /*return*/];
                }
            });
        });
    };
    HasuraBackgroundClient.prototype.insertAssets = function (assets) {
        return __awaiter(this, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.logger.debug({ module: 'HasuraClient', qty: assets.length }, 'inserting assets found in tokens');
                        return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_6 || (templateObject_6 = __makeTemplateObject(["mutation InsertAssets($assets: [Asset_insert_input!]!) {\n          insert_assets(objects: $assets) {\n              returning {\n                  name\n                  policyId\n                  description\n                  assetName\n                  assetId\n              }\n              affected_rows\n          }\n      }"], ["mutation InsertAssets($assets: [Asset_insert_input!]!) {\n          insert_assets(objects: $assets) {\n              returning {\n                  name\n                  policyId\n                  description\n                  assetName\n                  assetId\n              }\n              affected_rows\n          }\n      }"]))), {
                                assets: assets.map(function (asset) { return (__assign(__assign({}, asset), {
                                    assetId: withHexPrefix(asset.assetId),
                                    assetName: withHexPrefix(asset.assetName),
                                    policyId: withHexPrefix(asset.policyId)
                                })); })
                            })];
                    case 1:
                        result = _a.sent();
                        return [2 /*return*/, result];
                }
            });
        });
    };
    HasuraBackgroundClient.prototype.getAssetMetadataHashesById = function (assetIds) {
        return __awaiter(this, void 0, void 0, function () {
            var result;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this.client.request(graphql_request_1.gql(templateObject_7 || (templateObject_7 = __makeTemplateObject(["query AssetMetadataHashes (\n          $assetIds: [bytea!]!\n      ){\n          assets (\n              where: {\n                  assetId: { _in: $assetIds }\n              }) {\n              assetId\n              metadataHash\n          }\n      }"], ["query AssetMetadataHashes (\n          $assetIds: [bytea!]!\n      ){\n          assets (\n              where: {\n                  assetId: { _in: $assetIds }\n              }) {\n              assetId\n              metadataHash\n          }\n      }"]))), {
                            assetIds: assetIds.map(function (id) { return withHexPrefix(id); })
                        })];
                    case 1:
                        result = _a.sent();
                        return [2 /*return*/, result.assets];
                }
            });
        });
    };
    return HasuraBackgroundClient;
}());
exports.HasuraBackgroundClient = HasuraBackgroundClient;
var templateObject_1, templateObject_2, templateObject_3, templateObject_4, templateObject_5, templateObject_6, templateObject_7;
