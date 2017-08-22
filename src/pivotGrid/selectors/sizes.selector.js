import { createSelector } from 'reselect';

import scrollbarSize from '../utils/scrollbarSize';
import { ROOT_ID, AxisType } from '../constants';
import { rowLeavesSelector, columnLeavesSelector } from './axis.selector';
import {
  columnDimensionsSelector,
  rowDimensionsSelector
} from './dimensions.selector';

export const defaultCellSizesSelector = createSelector(
  [
    state => state.config.cellHeight,
    state => state.config.cellWidth,
    state => state.config.zoom
  ],
  (cellHeight, cellWidth, zoom) => ({
    height: zoom * (cellHeight || 30),
    width: zoom * (cellWidth || 100),
    zoom
  })
);

export const getCellHeightByKeySelector = createSelector(
  [defaultCellSizesSelector, state => state.sizes.heights],
  (defaultSizes, heights) => key => {
    return defaultSizes.zoom * heights[key] || defaultSizes.height;
  }
);

export const getCellWidthByKeySelector = createSelector(
  [defaultCellSizesSelector, state => state.sizes.widths],
  (defaultSizes, widths) => key => {
    return defaultSizes.zoom * widths[key] || defaultSizes.width;
  }
);

// export const getColumnWidthSelector = createSelector(
//   [getCellWidthByKeySelector, columnLeavesSelector],
//   (getCellWidthByKey, columnLeaves) => ({ index }) =>
//     getCellWidthByKey(columnLeaves[index].key)
// );

export const getRowHeightSelector = createSelector(
  [getCellHeightByKeySelector, rowLeavesSelector],
  (getCellHeightByKeySelector, rowLeaves) => ({ index }) =>
    getCellHeightByKeySelector(rowLeaves[index].key)
);

export const getColumnWidthSelector = createSelector(
  [getCellWidthByKeySelector, columnLeavesSelector],
  (getCellWidthByKeySelector, columnLeaves) => ({ index }) =>
    getCellWidthByKeySelector(columnLeaves[index].key)
);

export const getColumnDimensionHeightSelector = createSelector(
  [getCellHeightByKeySelector, columnDimensionsSelector],
  (getCellHeightByKey, columnDimensions) => ({ index }) =>
    getCellHeightByKey(columnDimensions[index].id)
);

export const getRowDimensionWidthSelector = createSelector(
  [getCellWidthByKeySelector, rowDimensionsSelector],
  (getCellWidthByKey, rowDimensions) => ({ index }) =>
    getCellWidthByKey(rowDimensions[index].id)
);

const calculateCrossPositions = (dimensions, getCellSizeByKey) => {
  const res = {};
  let position = 0;
  // Total header if no dimensions
  // if (!dimensions.length) {
  //   position += getCrossSize(axisType, TOTAL_ID);
  // } else {
  if (dimensions.length === 0) {
    res[ROOT_ID] = { position, size: getCellSizeByKey(ROOT_ID) };
  } else {
    dimensions.forEach(dimension => {
      const size = getCellSizeByKey(dimension.id);
      res[dimension.id] = { position, size };
      position += size;
    });
  }
  return res;
};

export const crossPositionsSelector = createSelector(
  [
    getCellWidthByKeySelector,
    getCellHeightByKeySelector,
    columnDimensionsSelector,
    rowDimensionsSelector
  ],
  (getCellWidthByKey, getCellHeightByKey, columnDimensions, rowDimensions) => ({
    [AxisType.COLUMNS]: calculateCrossPositions(
      columnDimensions,
      getCellHeightByKey
    ),
    [AxisType.ROWS]: calculateCrossPositions(rowDimensions, getCellWidthByKey)
  })
);

//--------------------------------------------------------
export const rowsHeightSelector = createSelector(
  [rowLeavesSelector, getCellHeightByKeySelector],
  (rowLeaves, getCellHeightByKey) =>
    rowLeaves.reduce((height, leaf) => height + getCellHeightByKey(leaf.key), 0)
);
export const columnsWidthSelector = createSelector(
  [columnLeavesSelector, getCellWidthByKeySelector],
  (columnLeaves, getCellWidthByKey) =>
    columnLeaves.reduce((width, leaf) => width + getCellWidthByKey(leaf.key), 0)
);
export const rowHeadersWidthSelector = createSelector(
  [rowDimensionsSelector, getCellWidthByKeySelector],
  (rowDimensions, getCellWidthByKey) => {
    let width;
    if (rowDimensions.length === 0) {
      width = getCellWidthByKey(ROOT_ID);
    } else {
      width = rowDimensions.reduce(
        (width, dimension) => width + getCellWidthByKey(dimension.id),
        0
      );
    }
    return width;
  }
);
export const columnHeadersWidthSelector = createSelector(
  [columnDimensionsSelector, getCellHeightByKeySelector],
  (columnDimensions, getCellHeightByKey) => {
    let height;
    if (columnDimensions.length === 0) {
      height = getCellHeightByKey(ROOT_ID);
    } else {
      height = columnDimensions.reduce(
        (height, dimension) => height + getCellHeightByKey(dimension.id),
        0
      );
    }
    return height;
  }
);
const hasHorizontalScrollbar = createSelector(
  [
    state => state.config.width,
    state => state.config.height,
    columnsWidthSelector,
    rowHeadersWidthSelector
  ],
  (width, height, columnsWidth, rowHeadersWidth) =>
    width < columnsWidth + rowHeadersWidth + scrollbarSize()
);
const hasVerticalScrollbar = createSelector(
  [
    state => state.config.width,
    state => state.config.height,
    columnHeadersWidthSelector,
    rowsHeightSelector
  ],
  (width, height, columnHeadersHeight, rowsHeight) =>
    height < columnHeadersHeight + rowsHeight + scrollbarSize()
);

export const rowsVisibleHeightSelector = createSelector(
  [
    state => state.config.height,
    columnHeadersWidthSelector,
    rowsHeightSelector,
    hasHorizontalScrollbar
  ],
  (height, columnHeadersHeight, rowsHeight, hasScrollbar) =>
    Math.min(
      height - columnHeadersHeight - (hasScrollbar ? scrollbarSize() : 0),
      rowsHeight
    )
);
export const columnsVisibleWidthSelector = createSelector(
  [
    state => state.config.width,
    columnsWidthSelector,
    rowHeadersWidthSelector,
    hasVerticalScrollbar
  ],
  (width, columnsWidth, rowsWidth, hasScrollbar) =>
    Math.min(
      width - rowsWidth - (hasScrollbar ? scrollbarSize() : 0),
      columnsWidth
    )
);
export const previewSizesSelector = createSelector(
  [
    state => state.config.height,
    state => state.config.width,
    hasVerticalScrollbar,
    hasHorizontalScrollbar,
    rowsHeightSelector,
    rowHeadersWidthSelector,
    columnsWidthSelector,
    columnHeadersWidthSelector
  ],
  (
    height,
    width,
    hasVerticalScrollbar,
    hasHorizontalScrollbar,
    rowsHeight,
    rowHeadersWidth,
    columnsWidth,
    columnHeadersHeight
  ) => ({
    height: Math.min(
      height - (hasHorizontalScrollbar ? scrollbarSize() : 0),
      rowsHeight + columnHeadersHeight
    ),
    width: Math.min(
      width - (hasVerticalScrollbar ? scrollbarSize() : 0),
      columnsWidth + rowHeadersWidth
    )
  })
);

export const dataCellsHeightSelector = createSelector(
  [
    state => state.config.height,
    columnHeadersWidthSelector,
    rowsHeightSelector,
    hasHorizontalScrollbar
  ],
  (height, columnHeadersHeight, rowsHeight, hasScrollbar) =>
    Math.min(
      height - columnHeadersHeight,
      rowsHeight + (hasScrollbar ? scrollbarSize() : 0)
    )
);

export const dataCellsWidthSelector = createSelector(
  [
    state => state.config.width,
    columnsWidthSelector,
    rowHeadersWidthSelector,
    hasVerticalScrollbar
  ],
  (width, columnsWidth, rowHeadersWidth, hasScrollbar) =>
    Math.min(
      width - rowHeadersWidth,
      columnsWidth + (hasScrollbar ? scrollbarSize() : 0)
    )
);
const getLastChild = header => {
  let lastChild = header;
  if (lastChild.orderedChildrenIds && lastChild.orderedChildrenIds.length) {
    lastChild =
      lastChild.children[
        lastChild.orderedChildrenIds[lastChild.orderedChildrenIds.length - 1]
      ];
  }
  return lastChild;
};
export const getLastChildWidthSelector = createSelector(
  [getCellWidthByKeySelector],
  getWidth => header => getWidth(getLastChild(header).key)
);

export const getLastChildHeightSelector = createSelector(
  [getCellHeightByKeySelector],
  getHeight => header => getHeight(getLastChild(header).key)
);
