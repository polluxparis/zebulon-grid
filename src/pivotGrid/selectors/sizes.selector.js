// required sizes are (taking account of scrollbars size if needed)
//  data cells available sizes
//    - height
//    - width
//  row headers width
//  column headers height
//  + preview sizes or drag and drop
import { createSelector } from "reselect";
import { ROOT_ID, ScrollbarSize } from "../constants";
import {
  getCellWidthByKeySelector,
  getCellHeightByKeySelector
} from "./cellSizes.selector";
import {
  columnVisibleDimensionsSelector,
  rowVisibleDimensionsSelector
} from "./dimensions.selector";
import {
  getRowHeadersSelector,
  getColumnHeadersSelector
} from "./headers.selector";
console.log(
  getRowHeadersSelector,
  getColumnHeadersSelector,
  columnVisibleDimensionsSelector
);
//-------------------------------------------
export const rowHeadersWidthSelector = createSelector(
  [rowVisibleDimensionsSelector, getCellWidthByKeySelector],
  (rowDimensions, getCellWidthByKey) => {
    const width = rowDimensions.reduce(
      (width, dimension) =>
        width + dimension.isVisible * getCellWidthByKey(dimension.id),
      0
    );
    return width;
  }
);
//----------------------------------------------
export const columnHeadersHeightSelector = createSelector(
  [columnVisibleDimensionsSelector, getCellHeightByKeySelector],
  (columnDimensions, getCellHeightByKey) => {
    return columnDimensions.reduce(
      (height, dimension) =>
        height + dimension.isVisible * getCellHeightByKey(dimension.id),
      0
    );
  }
);
//-------------------------------------------
// export const horizontalScrollbarSizeSelector = createSelector(
//   [
//     state => state.config.width,
//     rowHeadersWidthSelector,
//     columnHeadersPositionsAndSizesSelector
//   ],
//   (width, rowHeadersWidth, columnHeadersPositionsAndSizes) =>
//     width < rowHeadersWidth + columnHeadersPositionsAndSizes.size
//       ? scrollbarSize
//       : 0
// );
// //---------------------------------------------
// export const verticalScrollbarSizeSelector = createSelector(
//   [
//     state => state.config.height,
//     columnHeadersHeightSelector,
//     rowHeadersPositionsAndSizesSelector
//   ],
//   (height, columnHeadersHeight, rowHeadersPositionsAndSizes) =>
//     height < columnHeadersHeight + rowHeadersPositionsAndSizes.size
//       ? scrollbarSize
//       : 0
// );
// //---------------------------------------------
// export const scrollbarSizesSelector = createSelector(
//   [horizontalScrollbarSizeSelector, verticalScrollbarSizeSelector],
//   (horizontalScrollbarSize, verticalScrollbarSize) => ({
//     horizontal: horizontalScrollbarSize,
//     vertical: verticalScrollbarSize
//   })
// );

// //  size of the datacells grid
// export const dataCellsHeightSelector = createSelector(
//   [
//     state => state.config.height,
//     columnHeadersHeightSelector,
//     rowHeadersPositionsAndSizesSelector,
//     horizontalScrollbarSizeSelector
//   ],
//   (height, columnHeadersHeight, rowHeadersPositionsAndSizes, scrollbarSize) =>
//     Math.min(
//       height - columnHeadersHeight - scrollbarSize,
//       rowHeadersPositionsAndSizes.size
//     )
// );
// export const dataCellsWidthSelector = createSelector(
//   [
//     state => state.config.width,
//     rowHeadersWidthSelector,
//     columnHeadersPositionsAndSizesSelector,
//     verticalScrollbarSizeSelector
//   ],
//   (width, rowHeadersWidth, columnHeadersPositionsAndSizes, scrollbarSize) =>
//     Math.min(
//       width - rowHeadersWidth - scrollbarSize,
//       columnHeadersPositionsAndSizes.size
//     )
// );
