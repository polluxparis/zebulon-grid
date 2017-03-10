import { createSelector } from 'reselect';
import { twoArraysIntersect } from '../utils/generic';
import { getActivatedDataFields } from './fields.selector';
import { getFilteredData } from './data.selector';

const getIndexesIntersectionFromDimensions = (
  rowDimension,
  columnDimension
) => {
  if (!(rowDimension && columnDimension)) {
    return null;
  }
  const rowIndexes = rowDimension.isRoot ? null : rowDimension.rowIndexes;
  const columnIndexes = columnDimension.isRoot
    ? null
    : columnDimension.rowIndexes;
  let intersection;
  if (rowIndexes === null && columnIndexes === null) {
    // At initialization, both rowIndexes and columnIndexes are null
    intersection = null;
  } else if (rowIndexes === null) {
    intersection = columnIndexes;
  } else if (columnIndexes === null) {
    intersection = rowIndexes;
  } else {
    intersection = twoArraysIntersect(columnIndexes, rowIndexes);
  }
  const emptyIntersection = !intersection || intersection.length === 0;
  if (emptyIntersection) {
    return null;
  }
  return intersection;
};

export const getCellValue = createSelector([getFilteredData], data => (
  datafield,
  rowDimension,
  columnDimension,
  aggregateFunc = () => null
) => {
  const intersection = getIndexesIntersectionFromDimensions(
    rowDimension,
    columnDimension
  );
  return aggregateFunc(datafield.id, intersection, data);
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
  [getFilteredData],
  datasource => cell => {
    const value = cell.caption;
    const dimensions = getDimensionInfos(cell.rowDimension).concat(
      getDimensionInfos(cell.columnDimension)
    );
    const data = getDataRows(
      datasource,
      cell.rowDimension,
      cell.columnDimension
    );
    const measure = {
      caption: cell.datafield.caption,
      id: cell.datafield.id
    };
    return { value, dimensions, data, measure };
  }
);
