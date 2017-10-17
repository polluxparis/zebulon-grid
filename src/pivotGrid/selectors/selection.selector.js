import { createSelector } from "reselect";
import { rowLeavesSelector, columnLeavesSelector } from "./axis.selector2";
import {
  getCellValueSelector,
  getCellDimensionInfosSelector
} from "./cell.selector";
import {
  rowVisibleDimensionsSelector,
  columnVisibleDimensionsSelector,
  activatedMeasuresSelector
} from "./dimensions.selector";
import { getLeaves } from "../utils/headers";
import copy from "../services/copyService";

const getIndexfromKey = (leaves, key) =>
  leaves.leaves.findIndex(leaf => leaf.key === key);

export const getRowIndexFromKey = createSelector(
  [rowLeavesSelector],
  leaves => key => getIndexfromKey(leaves, key)
);
export const getColumnIndexFromKey = createSelector(
  [columnLeavesSelector],
  leaves => key => getIndexfromKey(leaves, key)
);

export const getSelectedColumnRangeSelector = createSelector(
  [getColumnIndexFromKey, rowLeavesSelector],
  (getColumnIndexFromKey, rowLeaves) => header => {
    const columnLeaves = getLeaves(header);
    const startIndex = getColumnIndexFromKey(columnLeaves[0].key);
    const stopIndex = startIndex + columnLeaves.length - 1;
    return {
      selectedCellStart: { columnIndex: startIndex, rowIndex: 0 },
      selectedCellEnd: {
        columnIndex: stopIndex,
        rowIndex: rowLeaves.leaves.length - 1
      }
    };
  }
);
export const getSelectedRowRangeSelector = createSelector(
  [getRowIndexFromKey, columnLeavesSelector],
  (getRowIndexFromKey, columnLeaves) => header => {
    const rowLeaves = getLeaves(header);
    const startIndex = getRowIndexFromKey(rowLeaves[0].key);
    const stopIndex = startIndex + rowLeaves.length - 1;
    return {
      selectedCellStart: { columnIndex: 0, rowIndex: startIndex },
      selectedCellEnd: {
        columnIndex: columnLeaves.leaves.length - 1,
        rowIndex: stopIndex
      }
    };
  }
);

export const selectedRangeSelector = createSelector(
  [state => state.selectedRange],
  selectedRange => selectedRange
);

export const copySelector = createSelector(
  [
    rowLeavesSelector,
    columnLeavesSelector,
    rowVisibleDimensionsSelector,
    columnVisibleDimensionsSelector,
    activatedMeasuresSelector,
    getCellValueSelector,
    getCellDimensionInfosSelector,
    selectedRangeSelector
  ],
  (
    rowLeaves,
    columnLeaves,
    rowDimensions,
    columnDimensions,
    measures,
    getCellValue,
    getCellDimensionInfos,
    selectedRange
  ) => () =>
    copy({
      selectedRange,
      columnLeaves,
      rowLeaves,
      rowDimensions: rowDimensions.filter(row => row.isVisible),
      columnDimensions: columnDimensions.filter(column => column.isVisible),
      measures,
      getCellValue,
      getCellDimensionInfos
    })
);
