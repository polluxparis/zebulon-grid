'use strict'

import * as utils from './orb.utils'
import { Dimension } from './orb.dimension'

/**
 * Axe types
 * @readonly
 * @enum {Number}
 */
export const AxeType = {
  COLUMNS: 1,
  ROWS: 2,
  DATA: 3
}

/**
 * Creates a new instance of an axe's dimensions list.
 * @class
 * @memberOf orb
 * @param  {array} pgrid - Parent pivot grid
 * @param  {orb.axe.Type} type - Axe type (rows, columns, data)
 */
export class Axe {

  constructor (pgrid, type) {
    /**
     * Parent pivot grid
     * @type {orb.pgrid}
     */
    this.pgrid = pgrid
    /**
     * Axe type (rows, columns, data)
     * @type {orb.axe.Type}
     */
    this.type = type
    /**
     * This axe dimension fields
     * @type {Array}
     */
    this.fields = []
    switch (type) {
      case AxeType.COLUMNS:
        this.fields = this.pgrid.config.columnFields
        break
      case AxeType.ROWS:
        this.fields = this.pgrid.config.rowFields
        break
      case AxeType.DATA:
        this.fields = this.pgrid.config.dataFields
        break
      default:
        this.fields = []
    }

    /**
     * Number of dimensions in this axe
     * @type {Number}
     */
    this.dimensionsCount = null
    /**
     * Root dimension
     * @type {orb.dimension}
     */
    this.root = null
  /**
   * Dimensions dictionary indexed by depth
   * @type {Object} Dictionary of (depth, arrays)
   */
  // this.dimensionsByDepth = null
  }

  update () {
    this.dimensionsCount = this.fields.length
    this.root = new Dimension(++this.dimid, null, null, null, this.dimensionsCount + 1, true, false)

    // this.dimensionsByDepth = {}
    // for (let depth = 1; depth <= this.dimensionsCount; depth++) {
    //     this.dimensionsByDepth[depth] = []
    // }

    // fill data
    this.fill()

    // initial sort
    for (let findex = 0; findex < this.fields.length; findex++) {
      var ffield = this.fields[findex]
      if (ffield.sort.order === 'asc' || ffield.sort.order === 'desc') {
        this.sort(ffield, true)
      }
    }
  }

  sort (field, donottoggle) {
    if (field != null) {
      if (donottoggle !== true) {
        if (field.sort.order !== 'asc') {
          field.sort.order = 'asc'
        } else {
          field.sort.order = 'desc'
        }
      }

      // var depth = this.dimensionsCount - this.getfieldindex(field)
      // var parenjs = depth === this.dimensionsCount ? [this.root] : this.dimensionsByDepth[depth + 1]
      var parenjs = [this.root]
      for (let i = 0; i < parenjs.length; i++) {
        if (field.sort.customfunc != null) {
          parenjs[i].values.sort(field.sort.customfunc)
        } else {
          parenjs[i].values.sort()
        }
        if (field.sort.order === 'desc') {
          parenjs[i].values.reverse()
        }
      }
    }
  }

  // flattenValues() {
  //     return this.dimensionsByDepth[1].map(function(dim) {
  //         var name = ''
  //         var currDim = dim
  //         while(!currDim.isRoot) {
  //             name = currDim.value + (name !== '' ? '-' + name : '')
  //             currDim = currDim.parent
  //         }
  //         return {
  //             name: name,
  //             dim: dim
  //         }
  //     }).sort(function(a, b) {
  //         if(a.name < b.name) return -1
  //         if(a.name > b.name) return 1
  //         return 0
  //     })
  // }

  getfieldindex (field) {
    for (let i = 0; i < this.fields.length; i++) {
      if (this.fields[i].name === field.name) {
        return i
      }
    }
    return -1
  }

  /**
   * Creates all subdimensions using the supplied data
   * fill does two things:
   *   - filling the dimensionsByDepth array of the axe (used for sorting and flattenValues - note sure if useful)
   *   - filling the subdimvals array of each dimension of the axe
   *   - filling the rowIndexes array of each dimension of the axe (used for calculating aggregations)
   */
  fill () {
    if (this.pgrid.filteredDataSource != null && this.dimensionsCount > 0) {
      var datasource = this.pgrid.filteredDataSource
      if (datasource != null && utils.isArray(datasource) && datasource.length > 0) {
        for (let rowIndex = 0, dataLength = datasource.length; rowIndex < dataLength; rowIndex++) {
          var row = datasource[rowIndex]
          var dim = this.root
          for (let findex = 0; findex < this.dimensionsCount; findex++) {
            var depth = this.dimensionsCount - findex
            var subfield = this.fields[findex]
            var subvalue = row[subfield.name]
            var subdimvals = dim.subdimvals

            if (subdimvals[subvalue] !== undefined) {
              dim = subdimvals[subvalue]
            } else {
              dim.values.push(subvalue)
              dim = new Dimension(++this.dimid, dim, subvalue, subfield, depth, false, findex === this.dimensionsCount - 1)
              subdimvals[subvalue] = dim
              dim.rowIndexes = []
            // this.dimensionsByDepth[depth].push(dim)
            }

            dim.rowIndexes.push(rowIndex)
          }
        }
      }
    }
  // var dim = this.root
  // for (let findex = 0; findex < this.dimensionsCount; findex++) {
  //   var depth = this.dimensionsCount - findex
  //   var subfield = this.fields[findex]
  //   var dimMap = this.pgrid.dataSourceMap[subfield.name]
  //   Object.keys(dimMap).forEach(k => {
  //     dim.subdimvals.push(new Dimension(++this.dimid, dim, k, subfield, depth, false, findex == this.dimensionsCount - 1))
  //     dim.rowIndexes = dimMap[k]
  //   })
  // }
  }

}
