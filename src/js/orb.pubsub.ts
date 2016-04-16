/**
 * @fileOverview Publish/Subscribe pattern implementation
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

import * as utils from './orb.utils'

/**
 * Creates a new instance of pubsub.
 * @class
 * @memberOf orb
 */
export class PubSub {
    private _topics = {};

    subscribe(topic, callback) {
        if(utils.isString(topic) && utils.isFunction(callback)) {
            this._topics[topic] = this._topics[topic] || [];
            this._topics[topic].push(callback);
        }
    };

    publish(topic /*, callback arguments */) {
        if(utils.isString(topic)) {
            utils.forEach(
                this._topics[topic],
                (callback, ...args) => callback.apply(null, [topic].concat(Array.prototype.slice.call(args, 1)))
                )
        };
    }
};
