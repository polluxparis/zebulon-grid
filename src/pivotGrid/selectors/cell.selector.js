import { createSelector } from 'reselect';
import { isNullOrUndefined, twoArraysIntersect, range } from '../utils/generic';
import { filteredDataSelector } from './data.selector';
import {
  activatedMeasuresSelector,
  rowDimensionsSelector,
  columnDimensionsSelector
} from './dimensions.selector';
import { rowLeavesSelector, columnLeavesSelector } from './axis.selector';
import { ALL, MEASURE_ID, HeaderType } from '../constants';

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

const cellDimensionInfos = (data, axisDimensions, axis, leaf, measures) => {
  let l = leaf;
  const depth = axisDimensions.length;
  let dimension;
  const dimensions = [];
  const row = isNullOrUndefined(leaf.dataIndexes)
    ? null
    : data[leaf.dataIndexes[0]];
  for (let index = depth - 1; index >= 0; index -= 1) {
    dimension = axisDimensions[index];

    // when a leaf is collapsed its parent  has a depth < leaf.depth -1
    // we  have to push an empty cell in this case
    if (index > l.depth) {
      dimensions.push({
        axis,
        dimension: {
          id: dimension.id,
          caption: dimension.caption,
          isCollapsed: true
        },
        cell: { id: null, caption: null }
      });
    } else if (dimension.id === MEASURE_ID) {
      dimensions.push({
        axis,
        dimension: {
          id: dimension.id,
          caption: 'measures',
          isCollapsed: false
        },
        cell: { id: l.id, caption: measures[l.id].caption }
      });
      l = l.parent;
    } else {
      dimensions.push({
        axis,
        dimension: {
          id: dimension.id,
          caption: dimension.caption,
          isCollapsed: false
        },
        cell: { id: l.id, caption: dimension.labelAccessor(row) }
      });

      l = l.parent;
    }
  }
  return dimensions.reverse();
};
export const getCellDimensionInfosSelector = createSelector(
  [filteredDataSelector],
  data => (axisDimensions, axis, leaf, measures, dimensions) => {
    return cellDimensionInfos(
      data,
      axisDimensions,
      axis,
      leaf,
      measures,
      dimensions
    );
  }
);

export const getCellInfosSelector = createSelector(
  [
    filteredDataSelector,
    rowLeavesSelector,
    columnLeavesSelector,
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
    const rowLeaf = rowLeaves[cell.rowIndex];
    const columnLeaf = columnLeaves[cell.columnIndex];
    let measure;
    if (rowLeaf.type === HeaderType.MEASURE) {
      measure = measures[rowLeaf.id];
    } else {
      measure = measures[columnLeaf.id];
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
      'rows',
      rowLeaf,
      measures
    );
    dimensions = dimensions.concat(
      cellDimensionInfos(
        data,
        columnDimensions,
        'columns',
        columnLeaf,
        measures
      )
    );
    const usedData = twoArraysIntersect(
      rowLeaf.dataIndexes,
      columnLeaf.dataIndexes
    ).map(x => data[x]);
    return { value, dimensions, data: usedData };
  }
);
