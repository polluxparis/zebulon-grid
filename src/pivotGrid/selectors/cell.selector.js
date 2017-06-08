import { createSelector } from 'reselect';
import { isNullOrUndefined, twoArraysIntersect, range } from '../utils/generic';
import { getFilteredData } from './data.selector';
import { ALL } from '../constants';

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

export const getCellValue = createSelector([getFilteredData], data => (
  accessor,
  rowDimension,
  columnDimension,
  aggregation = () => null
) => {
  const intersection = getIndexesIntersectionFromDimensions(
    rowDimension,
    columnDimension
  );
  const intersectionArray = intersection === ALL
    ? range(0, data.length - 1)
    : intersection;
  // Remove rows for which the accessor gives a null or undefined value
  // This allows better behaviour for cells which have a null value
  // for example getting an empty cell instead of zero
  const intersectionWithNonNullOrUndefinedValue = intersectionArray.filter(
    i => !isNullOrUndefined(accessor(data[i]))
  );
  // If we assume that all our measures are numerical we can be more strict
  // and keep only rows where the accessor gives a finite number
  // This removes Javascript operators weird behaviour with non finite number values.
  // Design decision to be made later.
  //  const intersectionWithNumericalValue = intersectionArray.filter(
  //   i => Number.isFinite(accessor(data[i]))
  // );
  return aggregation(accessor, intersectionWithNonNullOrUndefinedValue, data);
});

const getDataRows = (data, rowDimension, columnDimension) => {
  const intersection = getIndexesIntersectionFromDimensions(
    rowDimension,
    columnDimension
  );
  return intersection.map(index => data[index]);
};

const getDimensionInfos = dimensionArg => {
  if (dimensionArg.isRoot) {
    return [];
  }
  const dimension = {
    caption: dimensionArg.field.caption,
    id: dimensionArg.field.id
  };
  const cell = {
    caption: dimensionArg.caption,
    id: dimensionArg.id
  };
  return [{ dimension, cell }].concat(getDimensionInfos(dimensionArg.parent));
};

export const getCellInfos = createSelector(
  [getFilteredData, state => state.config.dataHeadersLocation],
  (datasource, dataHeadersLocation) => cell => {
    const value = cell.caption;
    const dimensions = getDimensionInfos(cell.rowDimension)
      .map((dim, index, dimensions) => ({
        ...dim,
        axis: 'rows',
        index: dimensions.length - 1 - index
      }))
      .concat(
        getDimensionInfos(
          cell.columnDimension
        ).map((dim, index, dimensions) => ({
          ...dim,
          axis: 'columns',
          index: dimensions.length - 1 - index
        }))
      );
    const data = getDataRows(
      datasource,
      cell.rowDimension,
      cell.columnDimension
    );
    const measure = {
      caption: cell.datafield.caption,
      id: cell.datafield.id,
      axis: dataHeadersLocation
    };
    return { value, dimensions, data, measure };
  }
);
