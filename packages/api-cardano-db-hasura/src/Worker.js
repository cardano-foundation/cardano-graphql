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
exports.Worker = void 0;
var util_1 = require("@cardano-graphql/util");
var object_hash_1 = require("object-hash");
var ts_log_1 = require("ts-log");
var pg_boss_1 = require("pg-boss");
var ASSET_METADATA_FETCH_INITIAL = 'asset-metadata-fetch-initial';
var ASSET_METADATA_FETCH_UPDATE = 'asset-metadata-fetch-update';
var SIX_HOURS = 21600;
var MODULE_NAME = 'Worker';
var Worker = /** @class */ (function () {
    function Worker(hasuraClient, logger, metadataFetchClient, queueConfig, options) {
        if (logger === void 0) { logger = ts_log_1.dummyLogger; }
        this.hasuraClient = hasuraClient;
        this.logger = logger;
        this.metadataFetchClient = metadataFetchClient;
        this.queueConfig = queueConfig;
        this.options = options;
        this.state = 'initialized';
    }
    Worker.prototype.start = function () {
        return __awaiter(this, void 0, void 0, function () {
            var subscriptionHandler;
            var _this = this;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'initialized') {
                            throw new util_1.errors.ModuleIsNotInitialized(MODULE_NAME, 'start');
                        }
                        this.logger.info({ module: MODULE_NAME }, 'Starting');
                        this.queue = new pg_boss_1["default"](__assign({ application_name: 'cardano-graphql' }, this.queueConfig));
                        subscriptionHandler = function (data) { return __awaiter(_this, void 0, void 0, function () {
                            var jobs, assetIds, fetchedMetadata, existingAssetMetadataHashes, _loop_1, this_1, _i, jobs_1, job;
                            var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k, _l, _m;
                            return __generator(this, function (_o) {
                                switch (_o.label) {
                                    case 0:
                                        if (!('length' in data)) return [3 /*break*/, 6];
                                        jobs = data;
                                        this.logger.debug({ module: MODULE_NAME, qty: jobs.length }, 'Processing jobs');
                                        assetIds = jobs.map(function (job) { return job.data.assetId; });
                                        return [4 /*yield*/, this.metadataFetchClient.fetch(assetIds)];
                                    case 1:
                                        fetchedMetadata = _o.sent();
                                        return [4 /*yield*/, this.hasuraClient.getAssetMetadataHashesById(assetIds)];
                                    case 2:
                                        existingAssetMetadataHashes = _o.sent();
                                        _loop_1 = function (job) {
                                            var assetId, metadata, existingAssetMetadataHashObj, metadataHash;
                                            return __generator(this, function (_a) {
                                                switch (_a.label) {
                                                    case 0:
                                                        assetId = job.data.assetId;
                                                        metadata = fetchedMetadata.find(function (item) { return item.subject === assetId; });
                                                        if (!(metadata === undefined)) return [3 /*break*/, 1];
                                                        this_1.logger.trace({ module: MODULE_NAME, assetId: assetId }, 'Metadata not found in registry. Will retry');
                                                        job.done(new Error("Metadata for asset " + assetId + " not found in registry"));
                                                        return [3 /*break*/, 5];
                                                    case 1:
                                                        existingAssetMetadataHashObj = existingAssetMetadataHashes.find(function (item) { return item.assetId === assetId; });
                                                        metadataHash = object_hash_1["default"](metadata);
                                                        if (!((existingAssetMetadataHashObj === null || existingAssetMetadataHashObj === void 0 ? void 0 : existingAssetMetadataHashObj.metadataHash) === metadataHash)) return [3 /*break*/, 2];
                                                        this_1.logger.trace({ module: MODULE_NAME, assetId: assetId }, 'Metadata from registry matches local');
                                                        return [3 /*break*/, 5];
                                                    case 2:
                                                        this_1.logger.trace({ module: MODULE_NAME, assetId: assetId }, 'Found metadata in registry');
                                                        return [4 /*yield*/, this_1.hasuraClient.addAssetMetadata({
                                                                assetId: assetId,
                                                                decimals: (_a = metadata.decimals) === null || _a === void 0 ? void 0 : _a.value,
                                                                description: (_b = metadata.description) === null || _b === void 0 ? void 0 : _b.value,
                                                                logo: (_c = metadata.logo) === null || _c === void 0 ? void 0 : _c.value,
                                                                name: (_d = metadata.name) === null || _d === void 0 ? void 0 : _d.value,
                                                                ticker: (_e = metadata.ticker) === null || _e === void 0 ? void 0 : _e.value,
                                                                url: (_f = metadata.url) === null || _f === void 0 ? void 0 : _f.value,
                                                                metadataHash: metadataHash
                                                            })];
                                                    case 3:
                                                        _a.sent();
                                                        return [4 /*yield*/, this_1.queue.publishAfter(ASSET_METADATA_FETCH_UPDATE, { assetId: assetId }, {
                                                                retryDelay: (_j = (_h = (_g = this_1.options) === null || _g === void 0 ? void 0 : _g.metadataUpdateInterval) === null || _h === void 0 ? void 0 : _h.assets) !== null && _j !== void 0 ? _j : SIX_HOURS
                                                            }, (_m = (_l = (_k = this_1.options) === null || _k === void 0 ? void 0 : _k.metadataUpdateInterval) === null || _l === void 0 ? void 0 : _l.assets) !== null && _m !== void 0 ? _m : SIX_HOURS)];
                                                    case 4:
                                                        _a.sent();
                                                        _a.label = 5;
                                                    case 5: return [2 /*return*/];
                                                }
                                            });
                                        };
                                        this_1 = this;
                                        _i = 0, jobs_1 = jobs;
                                        _o.label = 3;
                                    case 3:
                                        if (!(_i < jobs_1.length)) return [3 /*break*/, 6];
                                        job = jobs_1[_i];
                                        return [5 /*yield**/, _loop_1(job)];
                                    case 4:
                                        _o.sent();
                                        _o.label = 5;
                                    case 5:
                                        _i++;
                                        return [3 /*break*/, 3];
                                    case 6: return [2 /*return*/];
                                }
                            });
                        }); };
                        return [4 /*yield*/, this.queue.start()];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, this.queue.subscribe(ASSET_METADATA_FETCH_INITIAL, {
                                batchSize: 10000,
                                newJobCheckIntervalSeconds: 5
                            }, subscriptionHandler)];
                    case 2:
                        _a.sent();
                        return [4 /*yield*/, this.queue.subscribe(ASSET_METADATA_FETCH_UPDATE, {
                                batchSize: 10000,
                                newJobCheckIntervalSeconds: 5
                            }, subscriptionHandler)];
                    case 3:
                        _a.sent();
                        this.logger.info({ module: MODULE_NAME }, 'Started');
                        return [2 /*return*/];
                }
            });
        });
    };
    Worker.prototype.shutdown = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'running') {
                            throw new util_1.errors.ModuleIsNotInitialized(MODULE_NAME, 'shutdown');
                        }
                        this.logger.info({ module: MODULE_NAME }, 'Shutting down');
                        return [4 /*yield*/, Promise.all([
                                this.queue.unsubscribe(ASSET_METADATA_FETCH_INITIAL),
                                this.queue.unsubscribe(ASSET_METADATA_FETCH_UPDATE)
                            ])];
                    case 1:
                        _a.sent();
                        this.state = 'initialized';
                        this.logger.info({ module: MODULE_NAME }, 'Shutdown complete');
                        return [2 /*return*/];
                }
            });
        });
    };
    return Worker;
}());
exports.Worker = Worker;
