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
exports.Db = void 0;
var pg_listen_1 = require("pg-listen");
var ts_log_1 = require("ts-log");
var MODULE_NAME = 'Db';
var Db = /** @class */ (function () {
    function Db(config, logger) {
        if (logger === void 0) { logger = ts_log_1.dummyLogger; }
        this.logger = logger;
        this.state = null;
        this.pgSubscriber = pg_listen_1["default"](config, {
            parse: function (value) { return value; }
        });
    }
    Db.prototype.init = function (_a) {
        var onDbInit = _a.onDbInit, onDbSetup = _a.onDbSetup;
        return __awaiter(this, void 0, void 0, function () {
            var error_1;
            var _this = this;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0:
                        if (this.state !== null)
                            return [2 /*return*/];
                        this.state = 'initializing';
                        this.logger.info({ module: MODULE_NAME }, 'Initializing...');
                        this.pgSubscriber.events.on('connected', function () { return __awaiter(_this, void 0, void 0, function () {
                            return __generator(this, function (_a) {
                                switch (_a.label) {
                                    case 0:
                                        this.logger.debug({ module: MODULE_NAME }, 'pgSubscriber: Connected');
                                        return [4 /*yield*/, onDbSetup()];
                                    case 1:
                                        _a.sent();
                                        return [2 /*return*/];
                                }
                            });
                        }); });
                        this.pgSubscriber.events.on('reconnect', function (attempt) {
                            _this.logger.warn({ module: MODULE_NAME }, "pgSubscriber: Reconnecting attempt " + attempt);
                        });
                        this.pgSubscriber.events.on('error', function (error) {
                            _this.logger.error({ module: MODULE_NAME, err: error }, 'pgSubscriber');
                            process.exit(1);
                        });
                        this.pgSubscriber.notifications.on('cardano_db_sync_startup', function (payload) { return __awaiter(_this, void 0, void 0, function () {
                            var _a;
                            return __generator(this, function (_b) {
                                switch (_b.label) {
                                    case 0:
                                        _a = payload;
                                        switch (_a) {
                                            case 'init': return [3 /*break*/, 1];
                                            case 'db-setup': return [3 /*break*/, 3];
                                        }
                                        return [3 /*break*/, 5];
                                    case 1:
                                        this.logger.warn({ module: 'Db' }, 'pgSubscriber: cardano-db-sync-extended starting, schema will be reset');
                                        return [4 /*yield*/, onDbInit()];
                                    case 2:
                                        _b.sent();
                                        return [3 /*break*/, 6];
                                    case 3: return [4 /*yield*/, onDbSetup()];
                                    case 4:
                                        _b.sent();
                                        return [3 /*break*/, 6];
                                    case 5:
                                        this.logger.error({ module: MODULE_NAME }, "DbClient.pgSubscriber: Unknown message payload " + payload);
                                        _b.label = 6;
                                    case 6: return [2 /*return*/];
                                }
                            });
                        }); });
                        _b.label = 1;
                    case 1:
                        _b.trys.push([1, 4, , 5]);
                        return [4 /*yield*/, this.pgSubscriber.connect()];
                    case 2:
                        _b.sent();
                        return [4 /*yield*/, this.pgSubscriber.listenTo('cardano_db_sync_startup')];
                    case 3:
                        _b.sent();
                        this.state = 'initialized';
                        return [3 /*break*/, 5];
                    case 4:
                        error_1 = _b.sent();
                        this.logger.error({ err: error_1 });
                        return [3 /*break*/, 5];
                    case 5: return [2 /*return*/];
                }
            });
        });
    };
    Db.prototype.shutdown = function () {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (this.state !== 'initialized')
                            return [2 /*return*/];
                        this.logger.info({ module: MODULE_NAME }, 'Shutting down...');
                        return [4 /*yield*/, this.pgSubscriber.close()];
                    case 1:
                        _a.sent();
                        this.state = null;
                        this.logger.info({ module: MODULE_NAME }, 'Shut down');
                        return [2 /*return*/];
                }
            });
        });
    };
    return Db;
}());
exports.Db = Db;
