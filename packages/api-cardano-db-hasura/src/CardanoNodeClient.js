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
exports.CardanoNodeClient = void 0;
var p_retry_1 = require("p-retry");
var util_1 = require("@cardano-graphql/util");
var client_1 = require("@cardano-ogmios/client");
var ts_log_1 = require("ts-log");
var util_2 = require("./util");
var MODULE_NAME = 'CardanoNodeClient';
var CardanoNodeClient = /** @class */ (function () {
    function CardanoNodeClient(logger) {
        if (logger === void 0) { logger = ts_log_1.dummyLogger; }
        this.logger = logger;
        this.state = null;
    }
    CardanoNodeClient.prototype.getTipSlotNo = function () {
        return __awaiter(this, void 0, void 0, function () {
            var tip, slotNo;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.logger.info({ module: MODULE_NAME }, this.state);
                        if (this.state !== 'initialized') {
                            throw new util_1.errors.ModuleIsNotInitialized(MODULE_NAME, 'getTipSlotNo');
                        }
                        return [4 /*yield*/, this.stateQueryClient.networkTip()];
                    case 1:
                        tip = _a.sent();
                        this.logger.info({ module: MODULE_NAME, tip: tip }, tip);
                        slotNo = tip === 'origin' ? 0 : tip.slot;
                        this.logger.debug({ module: MODULE_NAME, slotNo: slotNo }, 'getTipSlotNo');
                        return [2 /*return*/, slotNo];
                }
            });
        });
    };
    // Todo: Include in Graph
    // public async getProtocolParams (): Promise<Schema.ProtocolParametersShelley> {
    //   if (this.state !== 'initialized') {
    //     throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'getProtocolParams')
    //   }
    //   const protocolParams = await this.stateQueryClient.currentProtocolParameters()
    //   this.logger.debug({ module: MODULE_NAME, protocolParams }, 'getProtocolParams')
    //   return protocolParams
    // }
    CardanoNodeClient.prototype.initialize = function (ogmiosConnectionConfig) {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== null)
                            return [2 /*return*/];
                        this.state = 'initializing';
                        this.logger.info({ module: MODULE_NAME }, 'Initializing. This can take a few minutes...');
                        this.serverHealthFetcher = this.serverHealthFetcher || new util_1.DataFetcher('ServerHealth', function () { return client_1.getServerHealth({ connection: client_1.createConnectionObject(ogmiosConnectionConfig) }); }, 5000, this.logger);
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                var e_1;
                                return __generator(this, function (_a) {
                                    switch (_a.label) {
                                        case 0:
                                            _a.trys.push([0, 2, , 3]);
                                            return [4 /*yield*/, this.serverHealthFetcher.initialize()];
                                        case 1:
                                            _a.sent();
                                            return [3 /*break*/, 3];
                                        case 2:
                                            e_1 = _a.sent();
                                            this.logger.info('Waiting for Ogmios to be ready...');
                                            throw e_1;
                                        case 3: return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.2,
                                retries: 100,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Establishing connection to Ogmios server', this.logger)
                            })];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                var interactionContext, _a, tip, e_2, _b;
                                var _this = this;
                                return __generator(this, function (_c) {
                                    switch (_c.label) {
                                        case 0: return [4 /*yield*/, util_2.createInteractionContextWithLogger(ogmiosConnectionConfig, this.logger, MODULE_NAME, function () { return __awaiter(_this, void 0, void 0, function () {
                                                return __generator(this, function (_a) {
                                                    switch (_a.label) {
                                                        case 0: return [4 /*yield*/, this.shutdown()];
                                                        case 1:
                                                            _a.sent();
                                                            return [4 /*yield*/, this.initialize(ogmiosConnectionConfig)];
                                                        case 2:
                                                            _a.sent();
                                                            return [2 /*return*/];
                                                    }
                                                });
                                            }); })];
                                        case 1:
                                            interactionContext = _c.sent();
                                            _a = this;
                                            return [4 /*yield*/, client_1.createLedgerStateQueryClient(interactionContext)];
                                        case 2:
                                            _a.stateQueryClient = _c.sent();
                                            _c.label = 3;
                                        case 3:
                                            _c.trys.push([3, 5, , 6]);
                                            return [4 /*yield*/, this.stateQueryClient.ledgerTip()];
                                        case 4:
                                            tip = _c.sent();
                                            return [3 /*break*/, 6];
                                        case 5:
                                            e_2 = _c.sent();
                                            this.logger.error({ module: MODULE_NAME }, 'Querying ledger tip not yet available. Wait for later epoch. Ogmios Error Message: ' + e_2.message);
                                            throw e_2;
                                        case 6:
                                            this.logger.info({ module: MODULE_NAME }, tip);
                                            _b = this;
                                            return [4 /*yield*/, client_1.createTransactionSubmissionClient(interactionContext)];
                                        case 7:
                                            _b.txSubmissionClient = _c.sent();
                                            return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.2,
                                retries: 100,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Establishing connection to cardano-node', this.logger)
                            })];
                    case 2:
                        _a.sent();
                        this.state = 'initialized';
                        this.logger.info({ module: MODULE_NAME }, 'Initialized');
                        return [2 /*return*/];
                }
            });
        });
    };
    CardanoNodeClient.prototype.shutdown = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'initialized')
                            return [2 /*return*/];
                        this.logger.info({ module: MODULE_NAME }, 'Shutting down');
                        return [4 /*yield*/, this.serverHealthFetcher.shutdown()];
                    case 1:
                        _a.sent();
                        if (!(this.stateQueryClient.context.socket.readyState === this.stateQueryClient.context.socket.OPEN)) return [3 /*break*/, 3];
                        return [4 /*yield*/, this.stateQueryClient.shutdown()];
                    case 2:
                        _a.sent();
                        _a.label = 3;
                    case 3:
                        if (!(this.txSubmissionClient.context.socket.readyState === this.txSubmissionClient.context.socket.OPEN)) return [3 /*break*/, 5];
                        return [4 /*yield*/, this.txSubmissionClient.shutdown()];
                    case 4:
                        _a.sent();
                        _a.label = 5;
                    case 5:
                        this.state = null;
                        this.logger.info({ module: MODULE_NAME }, 'Shutdown');
                        return [2 /*return*/];
                }
            });
        });
    };
    CardanoNodeClient.prototype.submitTransaction = function (transaction) {
        return __awaiter(this, void 0, void 0, function () {
            var hash;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'initialized') {
                            throw new util_1.errors.ModuleIsNotInitialized(MODULE_NAME, 'submitTransaction');
                        }
                        if (this.serverHealthFetcher.value.networkSynchronization < 0.95) {
                            throw new util_1.errors.OperationRequiresSyncedNode('submitTransaction');
                        }
                        return [4 /*yield*/, this.txSubmissionClient.submitTransaction(transaction)];
                    case 1:
                        hash = _a.sent();
                        this.logger.info({ module: MODULE_NAME, hash: hash }, 'submitTransaction');
                        return [2 /*return*/, hash];
                }
            });
        });
    };
    return CardanoNodeClient;
}());
exports.CardanoNodeClient = CardanoNodeClient;
