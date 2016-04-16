/**
 * @fileOverview Publish/Subscribe pattern implementation
 * @author Najmeddine Nouri <najmno@gmail.com>
 */
'use strict';
/* global module, require */
/*jshint eqnull: true*/
var utils = require('./orb.utils');
/**
 * Creates a new instance of pubsub.
 * @class
 * @memberOf orb
 */
var PubSub = (function () {
    function PubSub() {
        this._topics = {};
    }
    PubSub.prototype.subscribe = function (topic, callback) {
        if (utils.isString(topic) && utils.isFunction(callback)) {
            this._topics[topic] = this._topics[topic] || [];
            this._topics[topic].push(callback);
        }
    };
    ;
    PubSub.prototype.publish = function (topic /*, callback arguments */) {
        if (utils.isString(topic)) {
            utils.forEach(this._topics[topic], function (callback) {
                var args = [];
                for (var _i = 1; _i < arguments.length; _i++) {
                    args[_i - 1] = arguments[_i];
                }
                return callback.apply(null, [topic].concat(Array.prototype.slice.call(args, 1)));
            });
        }
        ;
    };
    return PubSub;
}());
exports.PubSub = PubSub;
;
