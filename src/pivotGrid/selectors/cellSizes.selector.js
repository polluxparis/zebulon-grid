///////////////////////////////////////////////////////////////////
//  cell sizes by key (or row index or column index) selectors
//  cross dimensions sizes selector
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";
import { ROOT_ID, AxisType } from "../constants";
import { rowLeavesSelector, columnLeavesSelector } from "./axis.selector";
import {
  columnDimensionsSelector,
  rowDimensionsSelector
} from "./dimensions.selector";

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
//-------------------------------------------
export const getRowHeightSelector = createSelector(
  [getCellHeightByKeySelector, rowLeavesSelector],
  (getCellHeightByKeySelector, rowLeaves) => ({ index }) =>
    getCellHeightByKeySelector(rowLeaves[index].key)
);
//-------------------------------------------
export const getColumnWidthSelector = createSelector(
  [getCellWidthByKeySelector, columnLeavesSelector],
  (getCellWidthByKeySelector, columnLeaves) => ({ index }) =>
    getCellWidthByKeySelector(columnLeaves[index].key)
);
//-------------------------------------------
export const getColumnDimensionHeightSelector = createSelector(
  [getCellHeightByKeySelector, columnDimensionsSelector],
  (getCellHeightByKey, columnDimensions) => ({ index }) =>
    getCellHeightByKey(columnDimensions[index].id)
);
//-------------------------------------------
export const getRowDimensionWidthSelector = createSelector(
  [getCellWidthByKeySelector, rowDimensionsSelector],
  (getCellWidthByKey, rowDimensions) => ({ index }) =>
    getCellWidthByKey(rowDimensions[index].id)
);
//-------------------------------------------
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
