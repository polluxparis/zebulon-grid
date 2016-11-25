import { createSelector } from 'reselect';

import { Axis, AxisType } from '../Axis';
import AxisUi from '../AxisUi';
import { MEASURE_ID, TOTAL_ID } from '../constants';
import { twoArraysIntersect } from '../utils/generic';
import { scrollbarSize } from '../utils/domHelpers';

export const getCellSizes = createSelector(
  [
    state => state.config.cellHeight,
    state => state.config.cellWidth,
    state => state.config.zoom,
  ],
  (cellHeight, cellWidth, zoom) => ({
    height: zoom * cellHeight,
    width: zoom * cellWidth,
  }),
);

export const getRowFields = state => state.axis.rows.map(id => state.fields[id]);
export const getColumnFields = state => state.axis.columns.map(id => state.fields[id]);
export const getAvailableFields = state => state.axis.fields.map(id => state.fields[id]);
const getDataFields = state => state.datafields;

const getActivatedDataFields = createSelector(
  [getDataFields],
  datafields => Object.values(datafields).filter(field => field.activated),
);
const getActivatedDataFieldsCount = createSelector(
  [getActivatedDataFields],
  datafields => datafields.length,
);


const getFilters = state => state.filters;
const getData = state => state.data;

export const getFilteredData = createSelector(
  [getData, getFilters],
  (data, filters) => data,
);

export const getRowAxis = createSelector(
  [getRowFields, getFilteredData],
  (rowFields, filteredData) => new Axis(AxisType.ROWS, rowFields, filteredData),
);

export const getColumnAxis = createSelector(
  [getColumnFields, getFilteredData],
  (columnFields, filteredData) => new Axis(AxisType.COLUMNS, columnFields, filteredData),
);

export const getRowUiAxis = createSelector(
  [
    getRowAxis,
    getActivatedDataFields,
    getActivatedDataFieldsCount,
    state => state.config.dataHeadersLocation,
    state => state.axis.columns,
  ],
  (rowAxis, activatedDataFields, activatedDataFieldsCount, dataHeadersLocation, crossFieldsCode) =>
    new AxisUi(
      rowAxis,
      { activatedDataFields, activatedDataFieldsCount, dataHeadersLocation },
      crossFieldsCode),
);

export const getColumnUiAxis = createSelector(
  [
    getColumnAxis,
    getActivatedDataFields,
    state => state.config.dataHeadersLocation,
  ],
  (columnAxis, activatedDataFields, dataHeadersLocation) =>
    new AxisUi(columnAxis,
      {
        activatedDataFields,
        activatedDataFieldsCount: activatedDataFields.length,
        dataHeadersLocation,
      }),
);

export const getLayout = createSelector(
  [
    getRowAxis,
    getColumnAxis,
    getRowUiAxis,
    getColumnUiAxis,
    getActivatedDataFieldsCount,
    state => state.config.dataHeadersLocation,
  ],
  (rowAxis, columnAxis, rowsUi, columnsUi, activatedDataFieldsCount, dataHeadersLocation) => {
    const rowHorizontalCount = (rowAxis.fields.length || 1) +
      (dataHeadersLocation === 'rows' && activatedDataFieldsCount >= 1 ? 1 : 0);
    const rowVerticalCount = rowsUi.headers.length;
    const columnHorizontalCount = columnsUi.headers.length;
    const columnVerticalCount = (columnAxis.fields.length || 1) +
      (dataHeadersLocation === 'columns' && activatedDataFieldsCount >= 1 ? 1 : 0);
    return { rowHorizontalCount, rowVerticalCount, columnHorizontalCount, columnVerticalCount };
  },
);

const getLeafHeaderSize = (axis, key, sizes, cellSizes) => {
  if (axis === AxisType.COLUMNS) {
    return sizes.columns.leafs[key] || cellSizes.width;
  }
  return sizes.rows.leafs[key] || cellSizes.height;
};

export const getDimensionSize = createSelector(
  [
    state => state.sizes,
    getCellSizes,
  ],
  (sizes, cellSizes) => (axis, id) => {
    if (axis === AxisType.COLUMNS) {
      return sizes.columns.dimensions[id] || cellSizes.height;
    }
    return sizes.rows.dimensions[id] || cellSizes.width;
  },
);

export const getLastChildSize = createSelector(
  [
    state => state.sizes,
    getCellSizes,
  ],
  (sizes, cellSizes) => (axis, header) => {
    let lastChild = header;
    while (lastChild.subheaders && lastChild.subheaders.length) {
      lastChild = lastChild.subheaders[lastChild.subheaders.length - 1];
    }
    return getLeafHeaderSize(axis, header.key, sizes, cellSizes);
  },
);

const calculateDimensionPositions = (axis, fields, hasMeasures, getDimensionSize) => {
  let axisType;
  if (axis === 'columns') {
    axisType = 1;
  } else {
    axisType = 2;
  }
  const res = {};
  let position = 0;
  // Total header if no fields
  if (!fields.length) {
    position += getDimensionSize(axisType, TOTAL_ID);
  } else {
    fields.forEach((field) => {
      res[field.id] = position;
      position += getDimensionSize(axisType, field.id);
    });
  }
  if (hasMeasures) {
    res[MEASURE_ID] = position;
  }
  return res;
};


export const getDimensionPositions = createSelector(
  [
    getDimensionSize,
    state => state.config.dataHeadersLocation,
    getColumnFields,
    getRowFields,
  ],
  (getDimensionSize, dataHeadersLocation, columnFields, rowFields) => ({
    columns: calculateDimensionPositions('columns', columnFields, dataHeadersLocation === 'columns', getDimensionSize),
    rows: calculateDimensionPositions('rows', rowFields, dataHeadersLocation === 'rows', getDimensionSize),
  }),
);

export const getColumnWidth = createSelector(
  [
    getColumnUiAxis,
    state => state.sizes,
    getCellSizes,
  ],
  (columnsUi, sizes, cellSizes) => ({ index }) => {
    const headers = columnsUi.headers[index];
    const key = headers[headers.length - 1].key;
    return getLeafHeaderSize(AxisType.COLUMNS, key, sizes, cellSizes);
  },
);


export const getRowHeight = createSelector(
  [
    getRowUiAxis,
    state => state.sizes,
    getCellSizes,
  ],
  (rowsUi, sizes, cellSizes) => ({ index }) => {
    const headers = rowsUi.headers[index];
    const key = headers[headers.length - 1].key;
    return getLeafHeaderSize(AxisType.ROWS, key, sizes, cellSizes);
  },
);

export const getHeaderSizes = createSelector(
  [
    state => state.config.dataHeadersLocation,
    state => state.sizes,
    state => state.axis.rows,
    state => state.axis.columns,
    getRowUiAxis,
    getColumnUiAxis,
    getCellSizes,
    getDimensionSize,
  ],
  (dataHeadersLocation, sizes, rows, columns, rowsUi, columnsUi, cellSizes, getDimensionSize) => {
    const columnsMeasures = dataHeadersLocation === 'columns';
    let rowHeadersWidth = 0;
    // Measures are on the row axis
    if (!columnsMeasures) {
      rowHeadersWidth += sizes.rows.dimensions[MEASURE_ID] || cellSizes.width;
    }
    // There are no fields on the row axis
    if (!rows.length) {
      rowHeadersWidth += getDimensionSize(AxisType.ROWS, TOTAL_ID, sizes, cellSizes);
    } else {
      rowHeadersWidth = rows.reduce(
        (width, field) => width + getDimensionSize(AxisType.ROWS, field, sizes, cellSizes),
        rowHeadersWidth);
    }
    let columnHeadersHeight = 0;
    // Measures are on the column axis
    if (columnsMeasures) {
      columnHeadersHeight += sizes.columns.dimensions[MEASURE_ID] || cellSizes.height;
    }
    // There are no fields on the column axis
    if (!columns.length) {
      columnHeadersHeight += getDimensionSize(AxisType.COLUMNS, TOTAL_ID, sizes, cellSizes);
    } else {
      columnHeadersHeight = columns.reduce(
        (height, field) => height + getDimensionSize(AxisType.COLUMNS, field, sizes, cellSizes),
        columnHeadersHeight);
    }
    const rowHeadersHeight = rowsUi.headers.reduce(
      // (height, headers) => height + this.getRowHeight({ index: headers[0].x }),
      (height, headers) => height
      + getLeafHeaderSize(AxisType.ROWS, headers[headers.length - 1].key, sizes, cellSizes),
      0);
    const columnHeadersWidth = columnsUi.headers.reduce(
      // (width, headers) => width + this.getColumnWidth({ index: headers[0].x }),
      (width, headers) => width
      + getLeafHeaderSize(AxisType.COLUMNS, headers[headers.length - 1].key, sizes, cellSizes),
      0);
    return { rowHeadersWidth, columnHeadersWidth, rowHeadersHeight, columnHeadersHeight };
  },
);

export const getCellValue = createSelector(
  [
    getActivatedDataFields,
    getFilteredData,
  ],
  (activatedDataFields, data) => (datafield, rowDimension, columnDimension) => {
    if (!(rowDimension && columnDimension)) {
      return null;
    }
    const rowIndexes = rowDimension.isRoot ? null : rowDimension.getRowIndexes();
    const columnIndexes = columnDimension.isRoot ? null : columnDimension.getRowIndexes();
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
    return datafield.aggregateFunc(datafield.id, intersection, data);
  },
);

const hasScrollbar = createSelector(
  [
    state => state.config.width,
    state => state.config.height,
    getHeaderSizes,
  ],
  (
    width,
    height,
    { columnHeadersWidth, columnHeadersHeight, rowHeadersWidth, rowHeadersHeight },
  ) => ({
    bottom: width < columnHeadersWidth + rowHeadersWidth + scrollbarSize(),
    right: height < columnHeadersHeight + rowHeadersHeight + scrollbarSize(),
  }),
);

export const getRowHeadersVisibleHeight = createSelector(
  [
    state => state.config.height,
    getHeaderSizes,
    hasScrollbar,
  ],
  (height, { columnHeadersHeight, rowHeadersHeight }, hasScrollbar) =>
    Math.min(
      height - columnHeadersHeight - (hasScrollbar.bottom ? scrollbarSize() : 0),
      rowHeadersHeight,
    ),
);

export const getColumnHeadersVisibleWidth = createSelector(
  [
    state => state.config.width,
    getHeaderSizes,
    hasScrollbar,
  ],
  (width, { rowHeadersWidth, columnHeadersWidth }, hasScrollbar) =>
    Math.min(
      width - rowHeadersWidth - (hasScrollbar.right ? scrollbarSize() : 0),
      columnHeadersWidth,
    ),
);

export const getPreviewSizes = createSelector(
  [
    state => state.config.height,
    state => state.config.width,
    hasScrollbar,
    getHeaderSizes,
  ],
  (
    height,
    width,
    hasScrollbar,
    { rowHeadersHeight, rowHeadersWidth, columnHeadersHeight, columnHeadersWidth }) => ({
      height: Math.min(height - (hasScrollbar.bottom ? scrollbarSize() : 0),
        rowHeadersHeight + columnHeadersHeight),
      width: Math.min(width - (hasScrollbar.right ? scrollbarSize() : 0),
        columnHeadersWidth + rowHeadersWidth),
    }),
);

export const getDataCellsHeight = createSelector(
  [
    state => state.config.height,
    getHeaderSizes,
    hasScrollbar,
  ],
  (height, { columnHeadersHeight, rowHeadersHeight }, hasScrollbar) => Math.min(
    height - columnHeadersHeight,
    rowHeadersHeight + (hasScrollbar.bottom ? scrollbarSize() : 0),
  ),
);

export const getDataCellsWidth = createSelector(
  [
    state => state.config.width,
    getHeaderSizes,
    hasScrollbar,
  ],
  (width, { columnHeadersWidth, rowHeadersWidth }, hasScrollbar) => Math.min(
    width - rowHeadersWidth,
    columnHeadersWidth + (hasScrollbar.right ? scrollbarSize() : 0),
  ),
);
