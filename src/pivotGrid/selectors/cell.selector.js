import { createSelector } from 'reselect';
import {
  isNullOrUndefined,
  twoArraysIntersect,
  range,
  countHeadersDepth
} from '../utils/generic';
import { filteredDataSelector } from './data.selector';
import { HeaderType } from '../Cells';
import {
  activatedMeasuresSelector,
  rowDimensionsSelector,
  columnDimensionsSelector
} from './dimensions.selector';
import { rowLeavesSelector, getColumnLeaves } from './axis.selector';
import { ALL, ROOT_ID, MEASURE_ID } from '../constants';
import { Axis, AxisType, toAxisType } from '../Axis';
const getIndexesIntersectionFromDimensions = (
  rowDimension,
  columnDimension
) => {
  if (!(rowDimension && columnDimension)) {
    return [];
  }
  let intersection;
  if (rowDimension.isRoot && columnDimension.isRoot) {
    // When no dimension (i.e. Total) on both axis, intersection is ALL
    intersection = ALL;
  } else if (rowDimension.isRoot) {
    intersection = columnDimension.rowIndexes;
  } else if (columnDimension.isRoot) {
    intersection = rowDimension.rowIndexes;
  } else {
    intersection = twoArraysIntersect(
      columnDimension.rowIndexes,
      rowDimension.rowIndexes
    );
  }
  return intersection;
};

const cellValue = (
  data,
  valueAccessor,
  rowDataIndexes,
  columnDataIndexes,
  aggregation = () => null
) => {
  const intersection = twoArraysIntersect(rowDataIndexes, columnDataIndexes);
  const intersectionArray = intersection === ALL
    ? range(0, data.length - 1)
    : intersection;
  // Remove rows for which the accessor gives a null or undefined value
  // This allows better behaviour for cells which have a null value
  // for example getting an empty cell instead of zero
  const intersectionWithNonNullOrUndefinedValue = intersectionArray.filter(
    i => !isNullOrUndefined(valueAccessor(data[i]))
  );
  // If we assume that all our measures are numerical we can be more strict
  // and keep only rows where the accessor gives a finite number
  // This removes Javascript operators weird behaviour with non finite number values.
  // Design decision to be made later.
  //  const intersectionWithNumericalValue = intersectionArray.filter(
  //   i => Number.isFinite(accessor(data[i]))
  // );
  return aggregation(
    valueAccessor,
    intersectionWithNonNullOrUndefinedValue,
    data
  );
};

export const getCellValueSelector = createSelector(
  [filteredDataSelector],
  data => (
    valueAccessor,
    rowDataIndexes,
    columnDataIndexes,
    aggregation = () => null
  ) => {
    return cellValue(
      data,
      valueAccessor,
      rowDataIndexes,
      columnDataIndexes,
      aggregation
    );
  }
);

const getDataRows = (data, rowDimension, columnDimension) => {
  const intersection = getIndexesIntersectionFromDimensions(
    rowDimension,
    columnDimension
  );
  return intersection.map(index => data[index]);
};

// const getDimensionInfos = dimensionArg => {
//   if (dimensionArg.isRoot) {
//     return [];
//   }
//   const dimension = {
//     caption: dimensionArg.dimension.caption,
//     id: dimensionArg.dimension.id
//   };
//   const cell = {
//     caption: dimensionArg.caption,
//     id: dimensionArg.id
//   };
//   return [{ dimension, cell }].concat(getDimensionInfos(dimensionArg.parent));
// };

const cellDimensionInfos = (
  data,
  axisDimensions,
  hasMeasure,
  leaf,
  measures,
  dimensions
) => {
  let l = leaf;
  const depth = axisDimensions.length - hasMeasure;
  let dimension;
  const row = data[leaf.dataIndexes[0]];
  for (let index = depth - 1; index >= 0; index -= 1) {
    dimension = axisDimensions[index];
    if (dimension.id === MEASURE_ID) {
      dimensions.push({
        dimensionId: leaf.id,
        dimensionCaption: measures[leaf.id].caption,
        id: leaf.id,
        caption: measures[leaf.id].caption
      });
    } else {
      dimensions.push({
        dimensionId: dimension.id,
        dimensionCaption: dimension.caption,
        id: l.id,
        caption: dimension.labelAccessor(row)
      });
    }
    l = l.parent;
  }
  return dimensions.reverse();
};
export const getCellDimensionInfos = createSelector(
  [filteredDataSelector],
  data => (axisDimensions, hasMeasure, leaf, measures, dimensions) => {
    return cellDimensionInfos(
      data,
      axisDimensions,
      hasMeasure,
      leaf,
      measures,
      dimensions
    );
  }
);

export const getCellInfos = createSelector(
  [
    filteredDataSelector,
    rowLeavesSelector,
    getColumnLeaves,
    activatedMeasuresSelector,
    rowDimensionsSelector,
    columnDimensionsSelector
  ],
  (
    data,
    rowLeaves,
    columnLeaves,
    measures,
    rowDimensions,
    columnDimensions
  ) => cell => {
    let rowLeaf = rowLeaves[cell.rowIndex];
    let measure;
    let measureAxis;
    if (rowLeaf.type === HeaderType.MEASURE) {
      measure = measures[rowLeaf.id];
      rowLeaf = rowLeaf.parent;
      measureAxis = 'row';
    }
    let columnLeaf = columnLeaves[cell.columnIndex];
    if (columnLeaf.type === HeaderType.MEASURE) {
      measure = measures[columnLeaf.id];
      columnLeaf = columnLeaf.parent;
      measureAxis = 'column';
    }
    const value = cellValue(
      data,
      measure.valueAccessor,
      rowLeaf.dataIndexes,
      columnLeaf.dataIndexes,
      measure.aggregation
    );
    let dimensions = [];
    dimensions = cellDimensionInfos(
      data,
      rowDimensions,
      measureAxis === 'row',
      rowLeaf,
      dimensions
    );
    dimensions = cellDimensionInfos(
      data,
      columnDimensions,
      measureAxis === 'column',
      columnLeaf,
      dimensions
    );
    measure = {
      caption: measure.caption,
      id: measure.id,
      axis: measureAxis
    };
    return { value, dimensions, measure };
  }
);
