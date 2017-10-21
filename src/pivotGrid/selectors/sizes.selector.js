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
