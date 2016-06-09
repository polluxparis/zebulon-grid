'use strict'

import * as utils from './Utils'

/**
 * Creates a new instance of pubsub.
 * @class
 * @memberOf orb
 */
export class PubSub {
  constructor () {
    this._topics = {}
  }

  subscribe (topic, callback) {
    if (utils.isString(topic) && utils.isFunction(callback)) {
      this._topics[topic] = this._topics[topic] || []
      this._topics[topic].push(callback)
    }
  }

  publish (topic /*, callback arguments */) {
    if (utils.isString(topic)) {
      utils.forEach(
        this._topics[topic],
        (callback, ...args) => callback.apply(null, [topic].concat(args.slice(1)))
      )
    }
  }
}
