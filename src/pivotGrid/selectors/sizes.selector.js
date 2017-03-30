import { createSelector } from 'reselect';

import { scrollbarSize } from '../utils/domHelpers';
import { AxisType, toAxisType } from '../Axis';
import { MEASURE_ID, TOTAL_ID } from '../constants';
import { getColumnUiAxis, getRowUiAxis } from './axis.selector';
import { getColumnFields, getRowFields } from './fields.selector';

export const getCellSizes = createSelector(
  [
    state => state.config.cellHeight,
    state => state.config.cellWidth,
    state => state.config.zoom
  ],
  (cellHeight, cellWidth, zoom) => ({
    height: zoom * cellHeight,
    width: zoom * cellWidth
  })
);

export const getDimensionSize = createSelector(
  [
    state => state.sizes.dimensions.rows,
    state => state.sizes.dimensions.columns,
    getCellSizes
  ],
  (rowDimensions, columnDimensions, cellSizes) =>
    (axisType, id) => {
      if (axisType === AxisType.COLUMNS) {
        return columnDimensions[id] || cellSizes.height;
      }
      return rowDimensions[id] || cellSizes.width;
    }
);

const getLeafHeaderSize = (axis, key, leafSizes, cellSizes) => {
  if (axis === AxisType.COLUMNS) {
    return leafSizes[key] || cellSizes.width;
  }
  return leafSizes[key] || cellSizes.height;
};

export const getLastChildSizeOnColumns = createSelector(
  [state => state.sizes.leafs.columns, getCellSizes],
  (sizes, cellSizes) =>
    header => {
      let lastChild = header;
      while (lastChild.subheaders && lastChild.subheaders.length) {
        lastChild = lastChild.subheaders[lastChild.subheaders.length - 1];
      }
      return getLeafHeaderSize(
        AxisType.COLUMNS,
        lastChild.key,
        sizes,
        cellSizes
      );
    }
);

export const getLastChildSizeOnRows = createSelector(
  [state => state.sizes.leafs.rows, getCellSizes],
  (sizes, cellSizes) =>
    header => {
      let lastChild = header;
      while (lastChild.subheaders && lastChild.subheaders.length) {
        lastChild = lastChild.subheaders[lastChild.subheaders.length - 1];
      }
      return getLeafHeaderSize(AxisType.ROWS, lastChild.key, sizes, cellSizes);
    }
);

const calculateDimensionPositions = (
  axis,
  fields,
  hasMeasures,
  getDimensionSize
) => {
  const axisType = toAxisType(axis);
  const res = {};
  let position = 0;
  // Total header if no fields
  if (!fields.length) {
    position += getDimensionSize(axisType, TOTAL_ID);
  } else {
    fields.forEach(field => {
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
    getRowFields
  ],
  (getDimensionSize, dataHeadersLocation, columnFields, rowFields) => ({
    columns: calculateDimensionPositions(
      'columns',
      columnFields,
      dataHeadersLocation === 'columns',
      getDimensionSize
    ),
    rows: calculateDimensionPositions(
      'rows',
      rowFields,
      dataHeadersLocation === 'rows',
      getDimensionSize
    )
  })
);

export const getColumnWidth = createSelector(
  [getColumnUiAxis, state => state.sizes.leafs.columns, getCellSizes],
  (columnsUi, sizes, cellSizes) =>
    ({ index }) => {
      const headers = columnsUi.headers[index];
      const key = headers[headers.length - 1].key;
      return getLeafHeaderSize(AxisType.COLUMNS, key, sizes, cellSizes);
    }
);

export const getRowHeight = createSelector(
  [getRowUiAxis, state => state.sizes.leafs.rows, getCellSizes],
  (rowsUi, sizes, cellSizes) =>
    ({ index }) => {
      const headers = rowsUi.headers[index];
      const key = headers[headers.length - 1].key;
      return getLeafHeaderSize(AxisType.ROWS, key, sizes, cellSizes);
    }
);

export const getRowHeadersWidth = createSelector(
  [
    state => state.config.dataHeadersLocation,
    state => state.sizes.dimensions.rows,
    state => state.axis.rows,
    getCellSizes,
    getDimensionSize
  ],
  (dataHeadersLocation, sizes, rows, cellSizes, getDimensionSize) => {
    const columnsMeasures = dataHeadersLocation === 'columns';
    let rowHeadersWidth = 0;
    // Measures are on the row axis
    if (!columnsMeasures) {
      rowHeadersWidth += sizes[MEASURE_ID] || cellSizes.width;
    }
    // There are no fields on the row axis
    if (!rows.length) {
      rowHeadersWidth += getDimensionSize(AxisType.ROWS, TOTAL_ID);
    } else {
      rowHeadersWidth = rows.reduce(
        (width, field) => width + getDimensionSize(AxisType.ROWS, field),
        rowHeadersWidth
      );
    }
    return rowHeadersWidth;
  }
);

export const getColumnHeadersHeight = createSelector(
  [
    state => state.config.dataHeadersLocation,
    state => state.sizes.dimensions.columns,
    state => state.axis.columns,
    getCellSizes,
    getDimensionSize
  ],
  (dataHeadersLocation, sizes, columns, cellSizes, getDimensionSize) => {
    const columnsMeasures = dataHeadersLocation === 'columns';
    let columnHeadersHeight = 0;
    // Measures are on the column axis
    if (columnsMeasures) {
      columnHeadersHeight += sizes[MEASURE_ID] || cellSizes.height;
    }
    // There are no fields on the column axis
    if (!columns.length) {
      columnHeadersHeight += getDimensionSize(AxisType.COLUMNS, TOTAL_ID);
    } else {
      columnHeadersHeight = columns.reduce(
        (height, field) => height + getDimensionSize(AxisType.COLUMNS, field),
        columnHeadersHeight
      );
    }
    return columnHeadersHeight;
  }
);

export const getRowHeadersHeight = createSelector(
  [state => state.sizes.leafs.rows, getRowUiAxis, getCellSizes],
  (sizes, rowsUi, cellSizes) =>
    rowsUi.headers.reduce(
      (height, headers) =>
        height +
        getLeafHeaderSize(
          AxisType.ROWS,
          headers[headers.length - 1].key,
          sizes,
          cellSizes
        ),
      0
    )
);

export const getColumnHeadersWidth = createSelector(
  [state => state.sizes.leafs.columns, getColumnUiAxis, getCellSizes],
  (sizes, columnsUi, cellSizes) =>
    columnsUi.headers.reduce(
      // (width, headers) => width + this.getColumnWidth({ index: headers[0].x }),
      (width, headers) =>
        width +
        getLeafHeaderSize(
          AxisType.COLUMNS,
          headers[headers.length - 1].key,
          sizes,
          cellSizes
        ),
      0
    )
);

const hasHorizontalScrollbar = createSelector(
  [
    state => state.config.width,
    state => state.config.height,
    getColumnHeadersWidth,
    getRowHeadersWidth
  ],
  (width, height, columnHeadersWidth, rowHeadersWidth) =>
    width < columnHeadersWidth + rowHeadersWidth + scrollbarSize()
);
const hasVerticalScrollbar = createSelector(
  [
    state => state.config.width,
    state => state.config.height,
    getColumnHeadersHeight,
    getRowHeadersHeight
  ],
  (width, height, columnHeadersHeight, rowHeadersHeight) =>
    height < columnHeadersHeight + rowHeadersHeight + scrollbarSize()
);

export const getRowHeadersVisibleHeight = createSelector(
  [
    state => state.config.height,
    getColumnHeadersHeight,
    getRowHeadersHeight,
    hasHorizontalScrollbar
  ],
  (height, columnHeadersHeight, rowHeadersHeight, hasScrollbar) =>
    Math.min(
      height - columnHeadersHeight - (hasScrollbar ? scrollbarSize() : 0),
      rowHeadersHeight
    )
);

export const getColumnHeadersVisibleWidth = createSelector(
  [
    state => state.config.width,
    getRowHeadersWidth,
    getColumnHeadersWidth,
    hasVerticalScrollbar
  ],
  (width, rowHeadersWidth, columnHeadersWidth, hasScrollbar) =>
    Math.min(
      width - rowHeadersWidth - (hasScrollbar ? scrollbarSize() : 0),
      columnHeadersWidth
    )
);

export const getPreviewSizes = createSelector(
  [
    state => state.config.height,
    state => state.config.width,
    hasVerticalScrollbar,
    hasHorizontalScrollbar,
    getRowHeadersHeight,
    getRowHeadersWidth,
    getColumnHeadersHeight,
    getColumnHeadersWidth
  ],
  (height, width, hasVerticalScrollbar, hasHorizontalScrollbar, rowHeadersHeight, rowHeadersWidth, columnHeadersHeight, columnHeadersWidth) => ({
    height: Math.min(
      height - (hasHorizontalScrollbar ? scrollbarSize() : 0),
      rowHeadersHeight + columnHeadersHeight
    ),
    width: Math.min(
      width - (hasVerticalScrollbar ? scrollbarSize() : 0),
      columnHeadersWidth + rowHeadersWidth
    )
  })
);

export const getDataCellsHeight = createSelector(
  [
    state => state.config.height,
    getColumnHeadersHeight,
    getRowHeadersHeight,
    hasHorizontalScrollbar
  ],
  (height, columnHeadersHeight, rowHeadersHeight, hasScrollbar) =>
    Math.min(
      height - columnHeadersHeight,
      rowHeadersHeight + (hasScrollbar ? scrollbarSize() : 0)
    )
);

export const getDataCellsWidth = createSelector(
  [
    state => state.config.width,
    getColumnHeadersWidth,
    getRowHeadersWidth,
    hasVerticalScrollbar
  ],
  (width, columnHeadersWidth, rowHeadersWidth, hasScrollbar) =>
    Math.min(
      width - rowHeadersWidth,
      columnHeadersWidth + (hasScrollbar ? scrollbarSize() : 0)
    )
);