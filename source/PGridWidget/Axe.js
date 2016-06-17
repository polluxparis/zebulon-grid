'use strict'

import * as utils from './Utils'
import Dimension from './Dimension'

/**
 * Axe types
 * @readonly
 * @enum {Number}
 */
export const AxeType = {
  COLUMNS: 1,
  ROWS: 2,
  DATA: 3,
  FIELDS: 4
}

/**
 * Creates a new instance of an axe's dimensions list.
 * @class
 * @memberOf orb
 * @param  {array} store - Parent pivot grid
 * @param  {orb.axe.Type} type - Axe type (rows, columns, data)
 */
export class Axe {

  /**
   * Number of dimensions in this axe
   * @type {Number}
   */
  // dimensionsCount = null
  /**
   * Root dimension
   * @type {orb.dimension}
   */
  // root
/**
 * Dimensions dictionary indexed by depth
 * @type {Object} Dictionary of (depth, arrays)
 */
// this.dimensionsByDepth = null

  constructor (type, fields, store) {
    /**
     * Parent pivot grid
     * @type {orb.store}
     */
    this.store = store
    /**
     * Axe type (rows, columns, data)
     * @type {orb.axe.Type}
     */
    this.type = type
    /**
     * This axe dimension fields
     * @type {Array}
     */
    this.fields = fields
    // this.filters = this.getfilters()
    this.dimensionsCount = this.getdimensionsCount()
    this.root = this.getroot()
  }

  // getfilters () {
  //   return this.fields
  //     .filter(field => this.store.filters.has(field.name))
  //     .reduce((curr, res) => { res[curr.name] = this.store.filters.get(curr.name) }, {})
  // }

  getdimensionsCount () {
    return this.fields.length
  }

  getroot () {
    const dim = new Dimension(++this.dimid, null, null, null, this.dimensionsCount + 1, true, false)

    // this.dimensionsByDepth = {}
    // for (let depth = 1; depth <= this.dimensionsCount; depth++) {
    //     this.dimensionsByDepth[depth] = []
    // }

    // fill data
    this.fill(dim, this.store.filteredData)

    // initial sort
    for (let findex = 0; findex < this.fields.length; findex++) {
      var ffield = this.fields[findex]
      if (ffield.sort.order === 'asc' || ffield.sort.order === 'desc') {
        this.sort(dim, ffield, true)
      }
    }
    return dim
  }

  sort (root, field, donottoggle) {
    if (field != null) {
      if (donottoggle !== true) {
        if (field.sort.order !== 'asc') {
          field.sort.order = 'asc'
        } else {
          field.sort.order = 'desc'
        }
      }

      // var depth = this.dimensionsCount - this.getfieldindex(field)
      // var parents = depth === this.dimensionsCount ? [this.root] : this.dimensionsByDepth[depth + 1]
      var parents = [root]
      for (let i = 0; i < parents.length; i++) {
        if (field.sort.customfunc != null) {
          parents[i].values.sort(field.sort.customfunc)
        } else {
          parents[i].values.sort()
        }
        if (field.sort.order === 'desc') {
          parents[i].values.reverse()
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
  fill (root, data) {
    if (data != null && this.dimensionsCount > 0) {
      if (data != null && utils.isArray(data) && data.length > 0) {
        for (let rowIndex = 0, dataLength = data.length; rowIndex < dataLength; rowIndex++) {
          const row = data[rowIndex]
          var dim = root
          for (let findex = 0; findex < this.dimensionsCount; findex++) {
            const depth = this.dimensionsCount - findex
            const field = this.fields[findex]
            // const filter = this.store.filters.has(field.name) ? this.store.filters.get(field.name) : undefined
            const subvalue = row[field.name]
            const subdimvals = dim.subdimvals
            // if (filter && !filter.test(subvalue)) {
            //   break
            // } else {
              if (subdimvals[subvalue] !== undefined) {
                dim = subdimvals[subvalue]
              } else {
                dim.values.push(subvalue)
                dim = new Dimension(++this.dimid, dim, subvalue, field, depth, false, findex === this.dimensionsCount - 1)
                subdimvals[subvalue] = dim
                dim.rowIndexes = []
                // this.dimensionsByDepth[depth].push(dim)
              }
              dim.rowIndexes.push(rowIndex)
            // }
          }
        }
      }
      return root
    }
  // var dim = this.root
  // for (let findex = 0; findex < this.dimensionsCount; findex++) {
  //   var depth = this.dimensionsCount - findex
  //   var field = this.fields[findex]
  //   var dimMap = this.store.dataMap[field.name]
  //   Object.keys(dimMap).forEach(k => {
  //     dim.subdimvals.push(new Dimension(++this.dimid, dim, k, field, depth, false, findex == this.dimensionsCount - 1))
  //     dim.rowIndexes = dimMap[k]
  //   })
  // }
  }

}
