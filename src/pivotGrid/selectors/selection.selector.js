import { createSelector } from "reselect";
import { rowLeavesSelector, columnLeavesSelector } from "./axis.selector";
import {
  getCellValueSelector,
  getCellDimensionInfosSelector,
  buildDataSelector
} from "./cell.selector";
import {
  rowVisibleDimensionsSelector,
  columnVisibleDimensionsSelector,
  activatedMeasuresSelector
} from "./dimensions.selector";
import { getLeaves } from "../utils/headers";
import {
  copy,
  getSelectedElements,
  getSelectedText,
  getElementsToPaste,
  exportFile
} from "../services/copyService";

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
      selectedCellStart: { columns: startIndex, rows: 0 },
      selectedCellEnd: {
        columns: stopIndex,
        rows: rowLeaves.leaves.length - 1
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
      selectedCellStart: { columns: 0, rows: startIndex },
      selectedCellEnd: {
        columns: columnLeaves.leaves.length - 1,
        rows: stopIndex
      }
    };
  }
);

export const selectedRangeSelector = createSelector(
  [state => state.selectedRange],
  selectedRange => selectedRange
);

export const getElementsSelector = createSelector(
  [
    rowLeavesSelector,
    columnLeavesSelector,
    rowVisibleDimensionsSelector,
    columnVisibleDimensionsSelector,
    activatedMeasuresSelector,
    getCellValueSelector,
    getCellDimensionInfosSelector,
    state => state.configuration.measureHeadersAxis
  ],
  (
    rowLeaves,
    columnLeaves,
    rowDimensions,
    columnDimensions,
    measures,
    getCellValue,
    getCellDimensionInfos,
    measureHeadersAxis
  ) => range =>
    getSelectedElements({
      range,
      columnLeaves,
      rowLeaves,
      rowDimensions: rowDimensions.filter(row => row.isVisible),
      columnDimensions: columnDimensions.filter(column => column.isVisible),
      measures,
      getCellValue,
      getCellDimensionInfos,
      measureHeadersAxis
    })
);

export const copySelector = createSelector(
  [getElementsSelector],
  getElements => range => copy(getSelectedText(getElements(range)))
);
export const exportSelector = createSelector(
  [getElementsSelector],
  getElements => range =>
    exportFile(getSelectedText(getElements(range), "csv"), "toto.csv")
);
export const pasteSelector = createSelector(
  [
    rowLeavesSelector,
    columnLeavesSelector,
    activatedMeasuresSelector,
    getCellValueSelector,
    buildDataSelector
  ],
  (rowLeaves, columnLeaves, measures, getCellValue, buidData) => (
    clipboard,
    cell
  ) =>
    getElementsToPaste({
      rowLeaves,
      columnLeaves,
      measures,
      getCellValue,
      buidData,
      clipboard,
      cell
    })
);
