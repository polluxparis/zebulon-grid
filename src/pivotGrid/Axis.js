import Dimension from './Dimension';

/**
 * Axis types
 * @readonly
 * @enum {Number}
 */
export const AxisType = {
  COLUMNS: 1,
  ROWS: 2,
  DATA: 3,
  FIELDS: 4,
  CHART: 5,
};

/**
 * Creates a new instance of an axe's dimensions list.
 * @class
 * @memberOf orb
 * @param  {array} store - Parent pivot grid
 * @param  {orb.axe.Type} type - Axis type (rows, columns, data)
 */
export class Axis {

/**
 * Dimensions dictionary indexed by depth
 * @type {Object} Dictionary of (depth, arrays)
 */
// this.dimensionsByDepth = null

  constructor(type, fields, data) {
    /**
     * Axis type (rows, columns, data)
     * @type {orb.axe.Type}
     */
    this.type = type;

    /**
     * This axe dimension fields
     * @type {Array}
     */
    this.fields = fields;

    /**
     * Root dimension
     * @type {orb.dimension}
     */
    this.root = new Dimension(-1, null, null, null, this.fields.length + 1, true, false);
    this.fill(data);
    // initial sort
    this.fields.forEach((field) => {
      this.sort(field.id, true);
    });
  }

  sort(fieldId, doNotToggle) {
    const field = this.fields[this.getFieldIndex(fieldId)];
    if (doNotToggle !== true) {
      if (field.sort.order !== 'asc') {
        field.sort.order = 'asc';
      } else {
        field.sort.order = 'desc';
      }
    } else if (field.sort.order === null) {
      // If doNotToggle is true, fields without sort configuration are going to
      // be sorted in ascending order. This ensures that it is correctly recorded.
      field.sort.order = 'asc';
    }

    const depth = this.fields.length - this.getFieldIndex(field.id);
    let dimensions;
    if (depth === this.fields.length) {
      dimensions = [this.root];
    } else {
      dimensions = this.getDimensionsByDepth(depth + 1);
    }
    dimensions.forEach((dimension) => {
      if (field.sort.customfunc !== null) {
        dimension.values.sort(field.sort.customfunc);
      } else {
        dimension.values.sort();
      }
      if (field.sort.order === 'desc') {
        dimension.values.reverse();
      }
    });
  }

  // perhaps introduce a result parameter to obtain tail call optimisation
  getDimensionsByDepth(depth, dim = this.root) {
    // if (!dim) { dim = this.root; }
    if (depth === this.fields.length + 1) { return [dim]; }
    return [].concat(...Object.keys(dim.subdimvals)
      .map(dimValue => this.getDimensionsByDepth(depth + 1, dim.subdimvals[dimValue])));
  }

  getFieldIndex(fieldId) {
    return this.fields.map(fld => fld.id).indexOf(fieldId);
  }

  /**
   * Creates all subdimensions using the supplied data
   * fill does two things:
   *   - filling the dimensionsByDepth array of the axe
        (used for sorting and flattenValues - note sure if useful)
   *   - filling the subdimvals array of each dimension of the axe
   *   - filling the rowIndexes array of each dimension of the axe
        (used for calculating aggregations)
   */
  fill(data) {
    if (data != null && this.fields.length > 0) {
      if (data != null && Array.isArray(data) && data.length > 0) {
        for (let rowIndex = 0, dataLength = data.length; rowIndex < dataLength; rowIndex += 1) {
          const row = data[rowIndex];
          let dim = this.root;
          for (let findex = 0; findex < this.fields.length; findex += 1) {
            const depth = this.fields.length - findex;
            const field = this.fields[findex];
            const name = row[field.name];
            const id = row[field.id];
            const subdimvals = dim.subdimvals;
            if (subdimvals[id] !== undefined) {
              dim = subdimvals[id];
            } else {
              dim.values.push(id);
              dim = new Dimension(
                id,
                dim,
                name,
                field,
                depth,
                false,
                findex === this.fields.length - 1,
              );
              subdimvals[id] = dim;
              dim.rowIndexes = [];
            }
            dim.rowIndexes.push(rowIndex);
          }
        }
      }
    }
  }
}
