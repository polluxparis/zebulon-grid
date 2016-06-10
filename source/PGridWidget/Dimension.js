'use strict'

/**
 * Creates a new container for a row/column dimension values.<br/>
 * This object will have all informations related to a dimension: ijs values, depth, width and subdimensions.
 * @class
 * @memberOf orb
 * @param  {orb.dimension} parent - parent dimension
 * @param  {array} fields - array describing the fields used for an axe dimenisons
 * @param  {int} fieldindex - index of this dimension field in fields array
 * @param  {Boolean} isRoot - whether or not this is the root dimension for a given axe (row/column)
 */
export default class Dimension {

  constructor (id, parent, value, field, depth, isRoot, isLeaf) {
    /**
     * unique id within parent orb.axe instance.
     * @type {Number}
     */
    this.id = id
    /**
     * parent subdimension
     * @type {orb.dimension}
     */
    this.parent = parent
    /**
     * This instance dimension value
     * @type {object}
     */
    this.value = value
    /**
     * Whether or not this is the root dimension for a given axe (row/column)
     * @type {Boolean}
     */
    this.isRoot = isRoot
    /**
     * Whether or not this is the leaf (deepest) dimension for a given axe (row/column)
     * @type {Boolean}
     */
    this.isLeaf = isLeaf
    /**
     * Dimension's data field
     * @type {Array}
     */
    this.field = field
    /**
     * Dimension's depth (to the deepest sub-dimension)
     * @type {Number}
     */
    this.depth = depth
    /**
     * Dimension's set of all values
     * @type {Array}
     */
    this.values = []
    /**
     * Dimension's set of all values
     * @type {Array}
     */
    this.subdimvals = {}
    /**
     * Direct descendant subdimensions dictionary
     * @type {Object}
     */
    this.rowIndexes = null
  }

  getRowIndexes (result) {
    if (this.rowIndexes === null) {
      this.rowIndexes = []
      for (let i = 0; i < this.values.length; i++) {
        this.subdimvals[this.values[i]].getRowIndexes(this.rowIndexes)
      }
    }
    if (result != null) {
      for (let j = 0; j < this.rowIndexes.length; j++) {
        result.push(this.rowIndexes[j])
      }
      return result
    } else {
      return this.rowIndexes
    }
  }
}
