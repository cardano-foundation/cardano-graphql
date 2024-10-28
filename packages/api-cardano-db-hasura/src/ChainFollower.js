"use strict";
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
exports.ChainFollower = void 0;
var client_1 = require("@cardano-ogmios/client");
var p_retry_1 = require("p-retry");
var util_1 = require("@cardano-graphql/util");
var pg_boss_1 = require("pg-boss");
var ts_log_1 = require("ts-log");
var util_2 = require("./util");
var MODULE_NAME = 'ChainFollower';
var ChainFollower = /** @class */ (function () {
    function ChainFollower(hasuraClient, logger, queueConfig) {
        if (logger === void 0) { logger = ts_log_1.dummyLogger; }
        this.hasuraClient = hasuraClient;
        this.logger = logger;
        this.queueConfig = queueConfig;
        this.state = null;
    }
    ChainFollower.prototype.initialize = function (ogmiosConfig, getMostRecentPoint) {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== null)
                            return [2 /*return*/];
                        this.state = 'initializing';
                        this.logger.info({ module: MODULE_NAME }, 'Initializing');
                        this.queue = new pg_boss_1["default"](__assign({ application_name: 'cardano-graphql' }, this.queueConfig));
                        return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                                var context, _a;
                                var _this = this;
                                return __generator(this, function (_b) {
                                    switch (_b.label) {
                                        case 0: return [4 /*yield*/, util_2.createInteractionContextWithLogger(ogmiosConfig, this.logger, MODULE_NAME, function () { return __awaiter(_this, void 0, void 0, function () {
                                                var _a;
                                                return __generator(this, function (_b) {
                                                    switch (_b.label) {
                                                        case 0: return [4 /*yield*/, this.shutdown()];
                                                        case 1:
                                                            _b.sent();
                                                            return [4 /*yield*/, this.initialize(ogmiosConfig, getMostRecentPoint)];
                                                        case 2:
                                                            _b.sent();
                                                            _a = this.start;
                                                            return [4 /*yield*/, getMostRecentPoint()];
                                                        case 3: return [4 /*yield*/, _a.apply(this, [_b.sent()])];
                                                        case 4:
                                                            _b.sent();
                                                            return [2 /*return*/];
                                                    }
                                                });
                                            }); })];
                                        case 1:
                                            context = _b.sent();
                                            _a = this;
                                            return [4 /*yield*/, client_1.createChainSynchronizationClient(context, {
                                                    rollBackward: function (_a, requestNext) {
                                                        var point = _a.point, tip = _a.tip;
                                                        return __awaiter(_this, void 0, void 0, function () {
                                                            var deleteResult, deleteResult;
                                                            return __generator(this, function (_b) {
                                                                switch (_b.label) {
                                                                    case 0:
                                                                        if (!(point !== 'origin')) return [3 /*break*/, 2];
                                                                        this.logger.info({ module: MODULE_NAME, tip: tip, rollbackPoint: point }, 'Rolling back');
                                                                        return [4 /*yield*/, this.hasuraClient.deleteAssetsAfterSlot(point.slot.toString())];
                                                                    case 1:
                                                                        deleteResult = _b.sent();
                                                                        this.logger.info({ module: MODULE_NAME }, "Deleted " + deleteResult + " assets");
                                                                        return [3 /*break*/, 4];
                                                                    case 2:
                                                                        this.logger.info({ module: MODULE_NAME }, 'Rolling back to genesis');
                                                                        return [4 /*yield*/, this.hasuraClient.deleteAssetsAfterSlot('0')];
                                                                    case 3:
                                                                        deleteResult = _b.sent();
                                                                        this.logger.info({ module: MODULE_NAME }, "Deleted " + deleteResult + " assets");
                                                                        _b.label = 4;
                                                                    case 4:
                                                                        requestNext();
                                                                        return [2 /*return*/];
                                                                }
                                                            });
                                                        });
                                                    },
                                                    rollForward: function (_a, requestNext) {
                                                        var block = _a.block;
                                                        return __awaiter(_this, void 0, void 0, function () {
                                                            var b, _i, _b, tx, _c, _d, entry, policyId, assetNames, _e, assetNames_1, assetName;
                                                            return __generator(this, function (_f) {
                                                                switch (_f.label) {
                                                                    case 0:
                                                                        switch (block.type) {
                                                                            case 'praos':
                                                                                b = block;
                                                                                break;
                                                                            case 'bft':
                                                                                b = block;
                                                                                break;
                                                                            case 'ebb': // No transaction in there
                                                                                return [2 /*return*/];
                                                                        }
                                                                        if (!(b !== undefined && b.transactions !== undefined)) return [3 /*break*/, 8];
                                                                        _i = 0, _b = b.transactions;
                                                                        _f.label = 1;
                                                                    case 1:
                                                                        if (!(_i < _b.length)) return [3 /*break*/, 8];
                                                                        tx = _b[_i];
                                                                        if (!(tx.mint !== undefined)) return [3 /*break*/, 7];
                                                                        _c = 0, _d = Object.entries(tx.mint);
                                                                        _f.label = 2;
                                                                    case 2:
                                                                        if (!(_c < _d.length)) return [3 /*break*/, 7];
                                                                        entry = _d[_c];
                                                                        policyId = entry[0];
                                                                        assetNames = Object.keys(entry[1]);
                                                                        _e = 0, assetNames_1 = assetNames;
                                                                        _f.label = 3;
                                                                    case 3:
                                                                        if (!(_e < assetNames_1.length)) return [3 /*break*/, 6];
                                                                        assetName = assetNames_1[_e];
                                                                        return [4 /*yield*/, this.saveAsset(policyId, assetName, b)];
                                                                    case 4:
                                                                        _f.sent();
                                                                        _f.label = 5;
                                                                    case 5:
                                                                        _e++;
                                                                        return [3 /*break*/, 3];
                                                                    case 6:
                                                                        _c++;
                                                                        return [3 /*break*/, 2];
                                                                    case 7:
                                                                        _i++;
                                                                        return [3 /*break*/, 1];
                                                                    case 8:
                                                                        requestNext();
                                                                        return [2 /*return*/];
                                                                }
                                                            });
                                                        });
                                                    }
                                                })];
                                        case 2:
                                            _a.chainSyncClient = _b.sent();
                                            return [2 /*return*/];
                                    }
                                });
                            }); }, {
                                factor: 1.2,
                                retries: 100,
                                onFailedAttempt: util_1["default"].onFailedAttemptFor('Establishing connection to cardano-node chain-sync', this.logger)
                            })];
                    case 1:
                        _a.sent();
                        this.state = 'initialized';
                        this.logger.info({ module: MODULE_NAME }, 'Initialized');
                        return [2 /*return*/];
                }
            });
        });
    };
    ChainFollower.prototype.saveAsset = function (policyId, assetName, b) {
        return __awaiter(this, void 0, void 0, function () {
            var assetId, asset, SIX_HOURS, THREE_MONTHS;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        assetId = "" + policyId + (assetName !== undefined ? assetName : '');
                        return [4 /*yield*/, this.hasuraClient.hasAsset(assetId)];
                    case 1:
                        if (!!(_a.sent())) return [3 /*break*/, 4];
                        asset = {
                            assetId: assetId,
                            assetName: assetName,
                            firstAppearedInSlot: b.slot,
                            fingerprint: util_1.assetFingerprint(policyId, assetName),
                            policyId: policyId
                        };
                        return [4 /*yield*/, this.hasuraClient.insertAssets([asset])];
                    case 2:
                        _a.sent();
                        SIX_HOURS = 21600;
                        THREE_MONTHS = 365;
                        return [4 /*yield*/, this.queue.publish('asset-metadata-fetch-initial', { assetId: assetId }, {
                                retryDelay: SIX_HOURS,
                                retryLimit: THREE_MONTHS
                            })];
                    case 3:
                        _a.sent();
                        _a.label = 4;
                    case 4: return [2 /*return*/];
                }
            });
        });
    };
    ChainFollower.prototype.start = function (points) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'initialized') {
                            throw new util_1.errors.ModuleIsNotInitialized(MODULE_NAME, 'start');
                        }
                        this.logger.info({ module: MODULE_NAME }, 'Starting from ' + JSON.stringify(points));
                        return [4 /*yield*/, this.queue.start()];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, this.chainSyncClient.resume(points)];
                    case 2:
                        _a.sent();
                        this.state = 'running';
                        this.logger.info({ module: MODULE_NAME }, 'Started');
                        return [2 /*return*/];
                }
            });
        });
    };
    ChainFollower.prototype.shutdown = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'running') {
                            throw new util_1.errors.ModuleIsNotInitialized(MODULE_NAME, 'shutdown');
                        }
                        this.logger.info({ module: MODULE_NAME }, 'Shutting down');
                        return [4 /*yield*/, this.queue.stop()];
                    case 1:
                        _a.sent();
                        if (!(this.chainSyncClient.context.socket.readyState === this.chainSyncClient.context.socket.OPEN)) return [3 /*break*/, 3];
                        return [4 /*yield*/, this.chainSyncClient.shutdown()];
                    case 2:
                        _a.sent();
                        _a.label = 3;
                    case 3:
                        this.state = null;
                        this.logger.info({ module: MODULE_NAME }, 'Shutdown complete');
                        return [2 /*return*/];
                }
            });
        });
    };
    return ChainFollower;
}());
exports.ChainFollower = ChainFollower;
