/**
 * Creates a new container for a row/column dimension values.<br/>
 * This object will have all informations related to a dimension: ijs values, depth, width and subdimensions.
 * @class
 * @memberOf pivotgrid
 * @param  {pivotgrid.dimension} parent - parent dimension
 * @param  {array} fields - array describing the fields used for an axe dimenisons
 * @param  {int} fieldindex - index of this dimension field in fields array
 * @param  {Boolean} isRoot - whether or not this is the root dimension for a given axe (row/column)
 */
export default class Dimension {
  constructor(id, parent, caption, field, depth, isRoot, isLeaf) {
    /**
     * unique id within parent pivotgrid.axe instance.
     * @type {Number}
     */
    this.id = id;
    /**
     * parent subdimension
     * @type {pivotgrid.dimension}
     */
    this.parent = parent;
    /**
     * This instance dimension caption
     * @type {object}
     */
    this.caption = caption;
    /**
     * Whether or not this is the root dimension for a given axe (row/column)
     * @type {Boolean}
     */
    this.isRoot = isRoot;
    /**
     * Whether or not this is the leaf (deepest) dimension for a given axe (row/column)
     * @type {Boolean}
     */
    this.isLeaf = isLeaf;
    /**
     * Dimension's data field
     * @type {Array}
     */
    this.field = field;
    /**
     * Dimension's depth (to the deepest sub-dimension)
     * @type {Number}
     */
    this.depth = depth;
    /**
     * Dimension's set of all values
     * @type {Array}
     */
    this.values = [];
    /**
     * Dimension's set of all values
     * @type {Array}
     */
    this.subdimvals = {};
    /**
     * Direct descendant subdimensions dictionary
     * @type {Object}
     */
    this.rowIndexes = null;
    this.sortingMap = {};
  }
}
