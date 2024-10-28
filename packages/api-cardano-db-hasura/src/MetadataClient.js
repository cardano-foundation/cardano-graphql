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
exports.MetadataClient = void 0;
var axios_1 = require("axios");
var util_1 = require("@cardano-graphql/util");
var ts_log_1 = require("ts-log");
var p_retry_1 = require("p-retry");
var MODULE_NAME = 'MetadataFetchClient';
var MetadataClient = /** @class */ (function () {
    function MetadataClient(metadataServerUri, logger) {
        if (logger === void 0) { logger = ts_log_1.dummyLogger; }
        this.metadataServerUri = metadataServerUri;
        this.logger = logger;
        this.state = null;
        this.axiosClient = axios_1["default"].create({
            baseURL: this.metadataServerUri
        });
    }
    MetadataClient.prototype.ensureLocalMetadataServerIsAvailable = function () {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                            var error_1;
                            var _a, _b;
                            return __generator(this, function (_c) {
                                switch (_c.label) {
                                    case 0:
                                        _c.trys.push([0, 2, , 3]);
                                        return [4 /*yield*/, this.axiosClient.get('/health')];
                                    case 1:
                                        _c.sent();
                                        return [3 /*break*/, 3];
                                    case 2:
                                        error_1 = _c.sent();
                                        if (error_1.code === 'ENOTFOUND') {
                                            this.logger.info('Waiting for TokenRegistry to be available');
                                            throw new util_1.errors.HostDoesNotExist('metadata server');
                                        }
                                        else if (((_a = error_1.response) === null || _a === void 0 ? void 0 : _a.status) === 400) { // Needed until TokenRegistry is updated
                                            this.logger.info('Token Registry is up');
                                        }
                                        else if (((_b = error_1.response) === null || _b === void 0 ? void 0 : _b.status) !== 404) {
                                            this.logger.info('Metadata Server unreachable.');
                                            throw error_1;
                                        }
                                        return [3 /*break*/, 3];
                                    case 3: return [2 /*return*/];
                                }
                            });
                        }); }, {
                            factor: 1.5,
                            retries: 10
                        })];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    MetadataClient.prototype.waitForLocalMetadataServerSynced = function () {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, p_retry_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                            var result, error_2;
                            var _a;
                            return __generator(this, function (_b) {
                                switch (_b.label) {
                                    case 0:
                                        _b.trys.push([0, 2, , 3]);
                                        return [4 /*yield*/, this.axiosClient.get('/health')];
                                    case 1:
                                        result = _b.sent();
                                        if (!result.data.synced) {
                                            this.logger.info('Metadata registry is still syncing. This can take up to 90 min...');
                                            throw new Error('');
                                        }
                                        return [3 /*break*/, 3];
                                    case 2:
                                        error_2 = _b.sent();
                                        if (((_a = error_2.response) === null || _a === void 0 ? void 0 : _a.status) === 400) {
                                            this.logger.info('external Registry is up and running'); // Needed until TokenRegistry is updated
                                        }
                                        else {
                                            throw new Error('');
                                        }
                                        return [3 /*break*/, 3];
                                    case 3: return [2 /*return*/];
                                }
                            });
                        }); }, {
                            factor: 1.5,
                            retries: 1000,
                            minTimeout: 60000 // first try after one minute
                        })];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    MetadataClient.prototype.fetch = function (assetIds) {
        return __awaiter(this, void 0, void 0, function () {
            var response, error_3;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'initialized') {
                            throw new util_1.errors.ModuleIsNotInitialized(MODULE_NAME, 'fetch');
                        }
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 3, , 4]);
                        return [4 /*yield*/, this.axiosClient.post('metadata/query', {
                                subjects: assetIds,
                                properties: [
                                    'decimals',
                                    'description',
                                    'logo',
                                    'name',
                                    'ticker',
                                    'url'
                                ]
                            })];
                    case 2:
                        response = _a.sent();
                        return [2 /*return*/, response.data.subjects];
                    case 3:
                        error_3 = _a.sent();
                        if (error_3.code === 'ENOTFOUND') {
                            this.logger.error({ err: error_3 });
                        }
                        else {
                            throw error_3;
                        }
                        return [3 /*break*/, 4];
                    case 4: return [2 /*return*/];
                }
            });
        });
    };
    MetadataClient.prototype.initialize = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== null)
                            return [2 /*return*/];
                        this.state = 'initializing';
                        this.logger.info({ module: MODULE_NAME }, 'Initializing');
                        return [4 /*yield*/, this.ensureLocalMetadataServerIsAvailable()];
                    case 1:
                        _a.sent();
                        this.logger.info({ module: MODULE_NAME }, 'Metadata Server is up and running. Checking Sync Status.');
                        return [4 /*yield*/, this.waitForLocalMetadataServerSynced()];
                    case 2:
                        _a.sent();
                        this.state = 'initialized';
                        this.logger.info({ module: MODULE_NAME }, 'Initialized');
                        return [2 /*return*/];
                }
            });
        });
    };
    return MetadataClient;
}());
exports.MetadataClient = MetadataClient;
