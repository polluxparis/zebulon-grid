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
  FIELDS: 4,
  CHART: 5
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

    /**
     * Number of dimensions in this axe
     * @type {Number}
     */
    this.dimensionsCount = this.fields.length

    /**
     * Root dimension
     * @type {orb.dimension}
     */
    this.root = new Dimension(-1, null, null, null, this.dimensionsCount + 1, true, false)
    this.fill(this.store.filteredData)
    // initial sort
    this.fields.forEach(field => field.sort.order === 'asc' || field.sort.order === 'desc' ? this.sort(field, true) : null)
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

      var depth = this.dimensionsCount - this.getfieldindex(field)
      var parents = depth === this.dimensionsCount ? [this.root] : this.getDimensionsByDepth(depth + 1)
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

  // perhaps introduce a result parameter to obtain tail call optimisation
  getDimensionsByDepth (depth, dim) {
    if (!dim) { dim = this.root }
    if (depth === this.dimensionsCount + 1) { return [dim] }
    return [].concat(...Object.keys(dim.subdimvals).map(dimValue => this.getDimensionsByDepth(depth + 1, dim.subdimvals[dimValue])))
  }

  getfieldindex (field) {
    return this.fields.map(fld => fld.name).indexOf(field.name)
  }

  /**
   * Creates all subdimensions using the supplied data
   * fill does two things:
   *   - filling the dimensionsByDepth array of the axe (used for sorting and flattenValues - note sure if useful)
   *   - filling the subdimvals array of each dimension of the axe
   *   - filling the rowIndexes array of each dimension of the axe (used for calculating aggregations)
   */
  fill (data) {
    if (data != null && this.dimensionsCount > 0) {
      if (data != null && utils.isArray(data) && data.length > 0) {
        for (let rowIndex = 0, dataLength = data.length; rowIndex < dataLength; rowIndex++) {
          const row = data[rowIndex]
          var dim = this.root
          for (let findex = 0; findex < this.dimensionsCount; findex++) {
            const depth = this.dimensionsCount - findex
            const field = this.fields[findex]
            const subvalue = row[field.name]
            const id = row[field.code]
            const subdimvals = dim.subdimvals
            if (subdimvals[subvalue] !== undefined) {
              dim = subdimvals[subvalue]
            } else {
              dim.values.push(subvalue)
              dim = new Dimension(id, dim, subvalue, field, depth, false, findex === this.dimensionsCount - 1)
              subdimvals[subvalue] = dim
              dim.rowIndexes = []
            }
            dim.rowIndexes.push(rowIndex)
          }
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

}
