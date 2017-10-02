// required sizes are (taking account of scrollbars size if needed)
//  data cells available sizes
//    - height
//    - width
//  row headers width
//  column headers height
//  + preview sizes or drag and drop
import { createSelector } from "reselect";

// import scrollbarSize from "../utils/scrollbarSize";
import { ROOT_ID, AxisType } from "../constants";
import {
  getCellWidthByKeySelector,
  getCellHeightByKeySelector
} from "./cellSizes.selector";
import {
  rowHeadersPositionsAndSizesSelector,
  columnHeadersPositionsAndSizesSelector
} from "./headers.selector";
import {
  columnDimensionsSelector,
  rowDimensionsSelector
} from "./dimensions.selector";

const scrollbarSize = 12;
//-------------------------------------------
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
//----------------------------------------------
export const columnHeadersHeightSelector = createSelector(
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
//-------------------------------------------
export const horizontalScrollbarSizeSelector = createSelector(
  [
    state => state.config.width,
    rowHeadersWidthSelector,
    columnHeadersPositionsAndSizesSelector
  ],
  (width, rowHeadersWidth, columnHeadersPositionsAndSizes) =>
    width < rowHeadersWidth + columnHeadersPositionsAndSizes.size
      ? scrollbarSize
      : 0
);
//---------------------------------------------
export const verticalScrollbarSizeSelector = createSelector(
  [
    state => state.config.height,
    columnHeadersHeightSelector,
    rowHeadersPositionsAndSizesSelector
  ],
  (height, columnHeadersHeight, rowHeadersPositionsAndSizes) =>
    height < columnHeadersHeight + rowHeadersPositionsAndSizes.size
      ? scrollbarSize
      : 0
);
//---------------------------------------------
export const scrollbarSizesSelector = createSelector(
  [horizontalScrollbarSizeSelector, verticalScrollbarSizeSelector],
  (horizontalScrollbarSize, verticalScrollbarSize) => ({
    horizontal: horizontalScrollbarSize,
    vertical: verticalScrollbarSize
  })
);

//  size of the datacells grid
export const dataCellsHeightSelector = createSelector(
  [
    state => state.config.height,
    columnHeadersHeightSelector,
    rowHeadersPositionsAndSizesSelector,
    horizontalScrollbarSizeSelector
  ],
  (height, columnHeadersHeight, rowHeadersPositionsAndSizes, scrollbarSize) =>
    Math.min(
      height - columnHeadersHeight - scrollbarSize,
      rowHeadersPositionsAndSizes.size
    )
);
export const dataCellsWidthSelector = createSelector(
  [
    state => state.config.width,
    rowHeadersWidthSelector,
    columnHeadersPositionsAndSizesSelector,
    verticalScrollbarSizeSelector
  ],
  (width, rowHeadersWidth, columnHeadersPositionsAndSizes, scrollbarSize) =>
    Math.min(
      width - rowHeadersWidth - scrollbarSize,
      columnHeadersPositionsAndSizes.size
    )
);
export const previewSizesSelector = createSelector(
  [
    state => state.config.height,
    state => state.config.width,
    verticalScrollbarSizeSelector,
    horizontalScrollbarSizeSelector,
    dataCellsHeightSelector,
    dataCellsWidthSelector,
    columnHeadersHeightSelector,
    rowHeadersWidthSelector
  ],
  (
    height,
    width,
    verticalScrollbarSize,
    horizontalScrollbarSize,
    dataCellsHeight,
    dataCellsWidth,
    columnHeadersHeight,
    rowHeadersWidth
  ) => ({
    height: Math.min(
      height - horizontalScrollbarSize,
      dataCellsHeight + columnHeadersHeight
    ),
    width: Math.min(
      width - verticalScrollbarSize,
      dataCellsWidth + rowHeadersWidth
    )
  })
);
