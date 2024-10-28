"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
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
var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
exports.__esModule = true;
exports.MissingConfig = void 0;
var bunyan_1 = require("bunyan");
var index_1 = require("./index");
var death_1 = require("death");
var ts_custom_error_1 = require("ts-custom-error");
var fs_extra_1 = require("fs-extra");
var AssetCreator_1 = require("./AssetCreator");
// Todo: Hoist to util package next major version
var MissingConfig = /** @class */ (function (_super) {
    __extends(MissingConfig, _super);
    function MissingConfig(message) {
        var _this = _super.call(this) || this;
        _this.message = message;
        return _this;
    }
    return MissingConfig;
}(ts_custom_error_1.CustomError));
exports.MissingConfig = MissingConfig;
function getConfig() {
    return __awaiter(this, void 0, void 0, function () {
        var env, db, _a, _b, _c, _d, error_1, chainfollower, postgres, selectedEnv;
        return __generator(this, function (_e) {
            switch (_e.label) {
                case 0:
                    env = filterAndTypecastEnvs(process.env);
                    if (!env.hasuraCliPath) {
                        throw new MissingConfig('HASURA_CLI_PATH env not set');
                    }
                    if (!env.hasuraCliExtPath) {
                        throw new MissingConfig('HASURA_CLI_EXT_PATH env not set');
                    }
                    if (!env.hasuraUri) {
                        throw new MissingConfig('HASURA_URI env not set');
                    }
                    if (!env.metadataServerUri) {
                        throw new MissingConfig('METADATA_SERVER_URI env not set');
                    }
                    if (!env.postgres.dbFile && !env.postgres.db) {
                        throw new MissingConfig('POSTGRES_DB_FILE or POSTGRES_DB env not set');
                    }
                    if (!env.postgres.host) {
                        throw new MissingConfig('POSTGRES_HOST env not set');
                    }
                    if (!env.postgres.passwordFile && !env.postgres.password) {
                        throw new MissingConfig('POSTGRES_PASSWORD_FILE or POSTGRES_PASSWORD env not set');
                    }
                    if (!env.postgres.port) {
                        throw new MissingConfig('POSTGRES_PORT env not set');
                    }
                    if (!env.postgres.userFile && !env.postgres.user) {
                        throw new MissingConfig('POSTGRES_USER_FILE or POSTGRES_USER env not set');
                    }
                    _e.label = 1;
                case 1:
                    _e.trys.push([1, 8, , 9]);
                    _a = {};
                    _b = env.postgres.db;
                    if (_b) return [3 /*break*/, 3];
                    return [4 /*yield*/, fs_extra_1["default"].readFile(env.postgres.dbFile, 'utf8')];
                case 2:
                    _b = (_e.sent()).toString().trim();
                    _e.label = 3;
                case 3:
                    _a.database = _b,
                        _a.host = env.postgres.host;
                    _c = env.postgres.password;
                    if (_c) return [3 /*break*/, 5];
                    return [4 /*yield*/, fs_extra_1["default"].readFile(env.postgres.passwordFile, 'utf8')];
                case 4:
                    _c = (_e.sent()).toString().trim();
                    _e.label = 5;
                case 5:
                    _a.password = _c,
                        _a.port = env.postgres.port;
                    _d = env.postgres.user;
                    if (_d) return [3 /*break*/, 7];
                    return [4 /*yield*/, fs_extra_1["default"].readFile(env.postgres.userFile, 'utf8')];
                case 6:
                    _d = (_e.sent()).toString().trim();
                    _e.label = 7;
                case 7:
                    db = (_a.user = _d,
                        _a);
                    return [3 /*break*/, 9];
                case 8:
                    error_1 = _e.sent();
                    throw new MissingConfig('Database configuration cannot be read');
                case 9:
                    if (env.chainfollower) {
                        chainfollower = {
                            id: env.chainfollower.id,
                            slot: env.chainfollower.slot
                        };
                    }
                    postgres = env.postgres, selectedEnv = __rest(env, ["postgres"]);
                    return [2 /*return*/, __assign(__assign({}, selectedEnv), { db: db,
                            chainfollower: chainfollower, loggerMinSeverity: env.loggerMinSeverity || 'info' })];
            }
        });
    });
}
function filterAndTypecastEnvs(env) {
    var _a = env, ASSET_METADATA_UPDATE_INTERVAL = _a.ASSET_METADATA_UPDATE_INTERVAL, HASURA_CLI_PATH = _a.HASURA_CLI_PATH, HASURA_CLI_EXT_PATH = _a.HASURA_CLI_EXT_PATH, HASURA_URI = _a.HASURA_URI, LOGGER_MIN_SEVERITY = _a.LOGGER_MIN_SEVERITY, METADATA_SERVER_URI = _a.METADATA_SERVER_URI, OGMIOS_HOST = _a.OGMIOS_HOST, OGMIOS_PORT = _a.OGMIOS_PORT, POSTGRES_DB = _a.POSTGRES_DB, POSTGRES_DB_FILE = _a.POSTGRES_DB_FILE, POSTGRES_HOST = _a.POSTGRES_HOST, POSTGRES_PASSWORD = _a.POSTGRES_PASSWORD, POSTGRES_PASSWORD_FILE = _a.POSTGRES_PASSWORD_FILE, POSTGRES_PORT = _a.POSTGRES_PORT, POSTGRES_USER = _a.POSTGRES_USER, POSTGRES_USER_FILE = _a.POSTGRES_USER_FILE, CHAIN_FOLLOWER_START_ID = _a.CHAIN_FOLLOWER_START_ID, CHAIN_FOLLOWER_START_SLOT = _a.CHAIN_FOLLOWER_START_SLOT;
    return {
        hasuraCliPath: HASURA_CLI_PATH,
        hasuraCliExtPath: HASURA_CLI_EXT_PATH,
        hasuraUri: HASURA_URI,
        loggerMinSeverity: LOGGER_MIN_SEVERITY,
        metadataServerUri: METADATA_SERVER_URI,
        metadataUpdateInterval: {
            assets: ASSET_METADATA_UPDATE_INTERVAL ? Number(ASSET_METADATA_UPDATE_INTERVAL) : undefined
        },
        ogmios: {
            host: OGMIOS_HOST,
            port: OGMIOS_PORT ? Number(OGMIOS_PORT) : undefined
        },
        postgres: {
            db: POSTGRES_DB,
            dbFile: POSTGRES_DB_FILE,
            host: POSTGRES_HOST,
            password: POSTGRES_PASSWORD,
            passwordFile: POSTGRES_PASSWORD_FILE,
            port: POSTGRES_PORT ? Number(POSTGRES_PORT) : undefined,
            user: POSTGRES_USER,
            userFile: POSTGRES_USER_FILE
        },
        chainfollower: {
            id: CHAIN_FOLLOWER_START_ID,
            slot: CHAIN_FOLLOWER_START_SLOT ? Number(CHAIN_FOLLOWER_START_SLOT) : undefined
        }
    };
}
(function () {
    var _a;
    return __awaiter(this, void 0, void 0, function () {
        var config, logger, hasuraBackgroundClient_1, chainFollower_1, metadataClient, worker_1, assetCreater_1, db_1, error_2;
        var _this = this;
        return __generator(this, function (_b) {
            switch (_b.label) {
                case 0: return [4 /*yield*/, getConfig()];
                case 1:
                    config = _b.sent();
                    logger = bunyan_1.createLogger({
                        name: 'background',
                        level: config.loggerMinSeverity
                    });
                    _b.label = 2;
                case 2:
                    _b.trys.push([2, 4, , 5]);
                    logger.info('Hallo');
                    hasuraBackgroundClient_1 = new index_1.HasuraBackgroundClient(config.hasuraCliPath, config.hasuraCliExtPath, config.hasuraUri, logger);
                    chainFollower_1 = new index_1.ChainFollower(hasuraBackgroundClient_1, logger, config.db);
                    metadataClient = new index_1.MetadataClient(config.metadataServerUri, logger);
                    worker_1 = new index_1.Worker(hasuraBackgroundClient_1, logger, metadataClient, config.db, {
                        metadataUpdateInterval: {
                            assets: (_a = config.metadataUpdateInterval) === null || _a === void 0 ? void 0 : _a.assets
                        }
                    });
                    assetCreater_1 = new AssetCreator_1.AssetCreator(logger);
                    db_1 = new index_1.Db(config.db, logger);
                    // const getChainSyncPoints = async (): Promise<PointOrOrigin[]> => {
                    //   const chainSyncPoint = (config.chainfollower) as Schema.Point
                    //   logger.info(chainSyncPoint)
                    //   const mostRecentPoint = await hasuraBackgroundClient.getMostRecentPointWithNewAsset()
                    //   if (mostRecentPoint !== null) {
                    //     if (chainSyncPoint.slot && chainSyncPoint.slot > mostRecentPoint.slot) {
                    //       return [chainSyncPoint, 'origin']
                    //     } else {
                    //       return [mostRecentPoint, 'origin']
                    //     }
                    //   } else if (chainSyncPoint.slot && chainSyncPoint.id) {
                    //     return [chainSyncPoint, 'origin']
                    //   } else {
                    //     return ['origin']
                    //   }
                    // }
                    return [4 /*yield*/, db_1.init({
                            onDbInit: function () { return hasuraBackgroundClient_1.shutdown(); },
                            onDbSetup: function () { return __awaiter(_this, void 0, void 0, function () {
                                var error_3;
                                return __generator(this, function (_a) {
                                    switch (_a.label) {
                                        case 0:
                                            _a.trys.push([0, 2, , 3]);
                                            // await hasuraBackgroundClient.initialize()
                                            // await metadataClient.initialize()
                                            // await chainFollower.initialize(config.ogmios, getChainSyncPoints)
                                            // await worker.start()
                                            // await chainFollower.start(await getChainSyncPoints())
                                            return [4 /*yield*/, assetCreater_1.initialize()];
                                        case 1:
                                            // await hasuraBackgroundClient.initialize()
                                            // await metadataClient.initialize()
                                            // await chainFollower.initialize(config.ogmios, getChainSyncPoints)
                                            // await worker.start()
                                            // await chainFollower.start(await getChainSyncPoints())
                                            _a.sent();
                                            return [3 /*break*/, 3];
                                        case 2:
                                            error_3 = _a.sent();
                                            logger.error(error_3.message);
                                            process.exit(1);
                                            return [3 /*break*/, 3];
                                        case 3: return [2 /*return*/];
                                    }
                                });
                            }); }
                        })];
                case 3:
                    // const getChainSyncPoints = async (): Promise<PointOrOrigin[]> => {
                    //   const chainSyncPoint = (config.chainfollower) as Schema.Point
                    //   logger.info(chainSyncPoint)
                    //   const mostRecentPoint = await hasuraBackgroundClient.getMostRecentPointWithNewAsset()
                    //   if (mostRecentPoint !== null) {
                    //     if (chainSyncPoint.slot && chainSyncPoint.slot > mostRecentPoint.slot) {
                    //       return [chainSyncPoint, 'origin']
                    //     } else {
                    //       return [mostRecentPoint, 'origin']
                    //     }
                    //   } else if (chainSyncPoint.slot && chainSyncPoint.id) {
                    //     return [chainSyncPoint, 'origin']
                    //   } else {
                    //     return ['origin']
                    //   }
                    // }
                    _b.sent();
                    death_1["default"](function () { return __awaiter(_this, void 0, void 0, function () {
                        return __generator(this, function (_a) {
                            switch (_a.label) {
                                case 0: return [4 /*yield*/, Promise.all([
                                        hasuraBackgroundClient_1.shutdown,
                                        worker_1.shutdown,
                                        chainFollower_1.shutdown,
                                        db_1.shutdown
                                    ])];
                                case 1:
                                    _a.sent();
                                    process.exit(1);
                                    return [2 /*return*/];
                            }
                        });
                    }); });
                    return [3 /*break*/, 5];
                case 4:
                    error_2 = _b.sent();
                    logger.error('Exiting due to uncaught exception', error_2.message);
                    process.exit(1);
                    return [3 /*break*/, 5];
                case 5: return [2 /*return*/];
            }
        });
    });
})();
