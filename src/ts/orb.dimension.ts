/**
 * @fileOverview Pivot Grid dimension viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module */
/*jshint eqnull: true*/

/**
 * Creates a new container for a row/column dimension values.<br/>
 * This object will have all informations related to a dimension: its values, depth, width and subdimensions.
 * @class
 * @memberOf orb
 * @param  {orb.dimension} parent - parent dimension
 * @param  {array} fields - array describing the fields used for an axe dimenisons
 * @param  {int} fieldindex - index of this dimension field in fields array
 * @param  {Boolean} isRoot - whether or not this is the root dimension for a given axe (row/column)
 */
export class Dimension{

    /**
     * unique id within parent orb.axe instance.
     * @type {Number}
     */
    public id;
    /**
     * parent subdimension
     * @type {orb.dimension}
     */
    public parent;
    /**
     * This instance dimension value
     * @type {object}
     */
    public value;
    /**
     * Whether or not this is the root dimension for a given axe (row/column)
     * @type {Boolean}
     */
    public isRoot;
    /**
     * Whether or not this is the leaf (deepest) dimension for a given axe (row/column)
     * @type {Boolean}
     */
    public isLeaf;
    /**
     * Dimension's data field
     * @type {Array}
     */
    public field;
    /**
     * Dimension's depth (to the deepest sub-dimension)
     * @type {Number}
     */
    public depth;
    /**
     * Dimension's set of all values
     * @type {Array}
     */
    public values = [];
    /**
     * Direct descendant subdimensions dictionary
     * @type {Object}
     */
    public subdimvals = {};

    public rowIndexes = null;


    constructor(id, parent, value, field, depth, isRoot, isLeaf) {
        this.id = id;
        this.parent = parent;
        this.value = value;
        this.isRoot = isRoot;
        this.isLeaf = isLeaf;
        this.field = field;
        this.depth = depth;
    };

    getRowIndexes(result?) {
        if (this.rowIndexes == null) {
            this.rowIndexes = [];
            for (let i = 0; i < this.values.length; i++) {
                this.subdimvals[this.values[i]].getRowIndexes(this.rowIndexes);
            }
        }
        if (result != null) {
            for (let j = 0; j < this.rowIndexes.length; j++) {
                result.push(this.rowIndexes[j]);
            }
            return result;
        } else {
            return this.rowIndexes;
        }
    }
};
