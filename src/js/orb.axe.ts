/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

/* global module, require */
/*jshint eqnull: true*/

'use strict';

import * as utils from './orb.utils';
import {Dimension} from './orb.dimension';

/**
 * Axe types
 * @readonly
 * @enum {Number}
 */
export const AxeType = {
    COLUMNS: 1,
    ROWS: 2,
    DATA: 3
};

/**
 * Creates a new instance of an axe's dimensions list.
 * @class
 * @memberOf orb
 * @param  {array} pgrid - Parent pivot grid
 * @param  {orb.axe.Type} type - Axe type (rows, columns, data)
 */
export class Axe {

    public dimid = 0;

    /**
     * Parent pivot grid
     * @type {orb.pgrid}
     */
    public pgrid;

    /**
     * Axe type (rows, columns, data)
     * @type {orb.axe.Type}
     */
    public type;

    /**
     * This axe dimension fields
     * @type {Array}
     */
    public fields;

    /**
     * Number of dimensions in this axe
     * @type {Number}
     */
    public dimensionsCount;

    /**
     * Root dimension
     * @type {orb.dimension}
     */
    public root;

    /**
     * Dimensions dictionary indexed by depth
     * @type {Object} Dictionary of (depth, arrays)
     */
    public dimensionsByDepth;

    constructor(pgrid, type) {
        this.pgrid = pgrid;
        this.type = type;
        switch (type){
             case AxeType.COLUMNS:
                this.fields = this.pgrid.config.columnFields;
            case AxeType.ROWS:
                this.fields = this.pgrid.config.rowFields;
            case AxeType.DATA:
                this.fields = this.pgrid.config.dataFields;
            default:
                this.fields = [];
        }

        this.dimensionsCount = null;
        this.root = null;
        this.dimensionsByDepth = null;
    }



    // if (pgrid != null && pgrid.config != null) {



    update() {
        this.dimensionsCount = this.fields.length;
        this.root = new Dimension(++this.dimid, null, null, null, this.dimensionsCount + 1, true, false);

        this.dimensionsByDepth = {};
        for (var depth = 1; depth <= this.dimensionsCount; depth++) {
            this.dimensionsByDepth[depth] = [];
        }

        // fill data
        this.fill();

        // initial sort
        for (var findex = 0; findex < this.fields.length; findex++) {
            var ffield = this.fields[findex];
            if (ffield.sort.order === 'asc' || ffield.sort.order === 'desc') {
                this.sort(ffield, true);
            }
        }
    };

    sort(field, donottoggle) {
        if (field != null) {
            if (donottoggle !== true) {
                if (field.sort.order !== 'asc') {
                    field.sort.order = 'asc';
                } else {
                    field.sort.order = 'desc';
                }
            }

            var depth = this.dimensionsCount - this.getfieldindex(field);
            var parents = depth === this.dimensionsCount ? [this.root] : this.dimensionsByDepth[depth + 1];
            for (var i = 0; i < parents.length; i++) {
                if(field.sort.customfunc != null){
                    parents[i].values.sort(field.sort.customfunc);
                } else {
                    parents[i].values.sort();
                }
                if (field.sort.order === 'desc') {
                    parents[i].values.reverse();
                }
            }
        }
    };

    flattenValues() {
        return this.dimensionsByDepth[1].map(function(dim) {
            var name = '';
            var currDim = dim;
            while(!currDim.isRoot) {
                name = currDim.value + (name !== '' ? '-' + name : '');
                currDim = currDim.parent;
            }
            return {
                name: name,
                dim: dim
            };
        }).sort(function(a, b) {
            if(a.name < b.name) return -1;
            if(a.name > b.name) return 1;
            return 0;
        });
    };
    // }

    getfieldindex(field) {
        for (var i = 0; i < this.fields.length; i++) {
            if (this.fields[i].name === field.name) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Creates all subdimensions using the supplied data
     */
    fill() {

        if (this.pgrid.filteredDataSource != null && this.dimensionsCount > 0) {

            var datasource = this.pgrid.filteredDataSource;
            if (datasource != null && utils.isArray(datasource) && datasource.length > 0) {
                for (var rowIndex = 0, dataLength = datasource.length; rowIndex < dataLength; rowIndex++) {
                    var row = datasource[rowIndex];
                    var dim = this.root;
                    for (var findex = 0; findex < this.dimensionsCount; findex++) {
                        var depth = this.dimensionsCount - findex;
                        var subfield = this.fields[findex];
                        var subvalue = row[subfield.name];
                        var subdimvals = dim.subdimvals;

                        if (subdimvals[subvalue] !== undefined) {
                            dim = subdimvals[subvalue];
                        } else {
                            dim.values.push(subvalue);
                            dim = new Dimension(++this.dimid, dim, subvalue, subfield, depth, false, findex == this.dimensionsCount - 1);
                            subdimvals[subvalue] = dim;
                            dim.rowIndexes = [];
                            this.dimensionsByDepth[depth].push(dim);
                        }

                        dim.rowIndexes.push(rowIndex);
                    }
                }
            }
        }
    }
};
