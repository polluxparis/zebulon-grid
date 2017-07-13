import { isUndefined } from './utils/generic';

/**
 * Axis types
 * @readonly
 * @enum {Number}
 */
export const AxisType = {
  COLUMNS: 1,
  ROWS: 2,
  MEASURE: 3,
  DIMENSION: 4
  // DATA: 3,
  // FIELDS: 4,
  // CHART: 5
};

export function toAxis(axisType) {
  switch (axisType) {
    case AxisType.COLUMNS:
      return 'columns';
    case AxisType.ROWS:
      return 'rows';
    case AxisType.MEASURES:
      return 'measures';
    case AxisType.DIMENSION:
      return 'dimensions';
    default:
      return '__AXIS_TYPE_UNKNOWN__';
  }
}

export function toAxisType(axis) {
  return AxisType[axis.toUpperCase()];
}

// /**
//  * Creates a new instance of an axi's dimensions list.
//  * @class
//  * @memberOf pivotgrid
//  * @param  {array} store - Parent pivot grid
//  * @param  {pivotgrid.axe.Type} type - Axis type (rows, columns, data)
//  */
// export class Axis {
//   /**
//  * Dimensions dictionary indexed by depth
//  * @type {Object} Dictionary of (depth, arrays)
//  */
//   // this.dimensionsByDepth = null

//   constructor(type, dimensions, data) {
//     /**
//      * Axis type (rows, columns, data)
//      * @type {pivotgrid.axe.Type}
//      */
//     this.type = type;

//     /**
//      * This axe dimension dimensions
//      * @type {Array}
//      */
//     this.dimensions = dimensions;

//     /**
//      * Root dimension
//      * @type {pivotgrid.dimension}
//      */
//     this.root = new Dimension(
//       -1,
//       null,
//       null,
//       null,
//       this.dimensions.length + 1,
//       true,
//       false
//     );
//     this.fill(data);
//     // initial sort
//     this.dimensions.forEach(dimension => {
//       this.sort(dimension.id, true);
//     });
//   }

//   sort(dimensionId, doNotToggle) {
//     const dimension = this.dimensions[this.getDimensionIndex(dimensionId)];
//     if (doNotToggle !== true) {
//       if (dimension.sort.order !== "asc") {
//         dimension.sort.order = "asc";
//       } else {
//         dimension.sort.order = "desc";
//       }
//     } else if (!dimension.sort.order) {
//       // If doNotToggle is true, dimensions without sort configuration are going to
//       // be sorted in ascending order. This ensures that it is correctly recorded.
//       dimension.sort.order = "asc";
//     }

//     const depth = this.dimensions.length - this.getDimensionIndex(dimension.id);
//     let dimensions;
//     if (depth === this.dimensions.length) {
//       dimensions = [this.root];
//     } else {
//       dimensions = this.getDimensionsByDepth(depth + 1);
//     }
//     dimensions.forEach(dimension => {
//       if (dimension.sort.order) {
//         if (Object.keys(dimension.sortingMap).length > 0) {
//           const sorted = Object.keys(dimension.sortingMap);
//           if (dimension.sort.custom) {
//             sorted.sort(dimension.sort.custom);
//           } else {
//             sorted.sort();
//           }
//           /* eslint-disable no-param-reassign */
//           dimension.values = sorted.map(
//             sortingValue => dimension.sortingMap[sortingValue]
//           );
//           if (dimension.sort.order === "desc") {
//             dimension.values.reverse();
//           }
//           /* eslint-enable */
//         }
//       }
//     });
//   }

//   // perhaps introduce a result parameter to obtain tail call optimisation
//   getDimensionsByDepth(depth, dim = this.root) {
//     // if (!dim) { dim = this.root; }
//     if (depth === this.dimensions.length + 1) {
//       return [dim];
//     }
//     return [].concat(
//       ...Object.keys(dim.subdimvals).map(dimValue =>
//         this.getDimensionsByDepth(depth + 1, dim.subdimvals[dimValue])
//       )
//     );
//   }

//   getDimensionIndex(dimensionId) {
//     return this.dimensions.map(fld => fld.id).indexOf(dimensionId);
//   }

//   /**
//    * Creates all subdimensions using the supplied data
//    * fill does two things:
//    *   - filling the dimensionsByDepth array of the axe
//         (used for sorting and flattenValues - note sure if useful)
//    *   - filling the subdimvals array of each dimension of the axe
//    *   - filling the rowIndexes array of each dimension of the axe
//         (used for calculating aggregations)
//    */

//   fill(data) {
//     if (data != null && this.dimensions.length > 0) {
//       if (Array.isArray(data) && data.length > 0) {
//         // Create sorting accessors
//         const sortingAccessors = [];
//         this.dimensions.forEach((dimension, index) => {
//           if (
//             !isUndefined(dimension.sort) &&
//             !isUndefined(dimension.sort.accessor)
//           ) {
//             sortingAccessors[index] = dimension.sort.accessor;
//           } else {
//             // If no sorting accessor is defined, use the dimension itself as sort
//             sortingAccessors[index] = dimension.accessor;
//           }
//         });
//         data.forEach((row, rowIndex) => {
//           let dim = this.root;
//           this.dimensions.forEach((dimension, index) => {
//             const depth = this.dimensions.length - index;
//             const label = dimension.labelAccessor(row);
//             const id = dimension.accessor(row);
//             const subdimvals = dim.subdimvals;
//             if (subdimvals[id] !== undefined) {
//               dim = subdimvals[id];
//             } else {
//               dim.values.push(id);
//               // Add value to be sorted on to the dimension if necessary
//               let sortingValue = null;
//               if (!isUndefined(sortingAccessors[index])) {
//                 sortingValue = sortingAccessors[index](row);
//                 dim.sortingMap[sortingValue] = id;
//               }
//               dim = new Dimension(
//                 id,
//                 dim,
//                 label,
//                 dimension,
//                 depth,
//                 false,
//                 index === this.dimensions.length - 1
//               );
//               subdimvals[id] = dim;
//               dim.rowIndexes = [];
//             }
//             dim.rowIndexes.push(rowIndex);
//           });
//         });
//       }
//     }
//   }

//   dimensionsDataIndexes(dimensions, data) {
//     const res = dimensions.reduce(
//       (acc, dimension) => ({
//         ...acc,
//         [dimension.id]: {}
//       }),
//       {}
//     );
//     if (data != null && dimensions.length > 0) {
//       if (Array.isArray(data) && data.length > 0) {
//         // Create sorting accessors
//         data.forEach((row, index) => {
//           dimensions.forEach(dimension => {
//             const id = dimension.accessor(row);
//             if (res[dimension.id][id] === undefined) {
//               res[dimension.id][id] = [index];
//             } else {
//               res[dimension.id][id].push(index);
//             }
//           });
//         });
//       }
//     }
//   }
// }
