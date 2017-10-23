import {
  SELECT_RANGE,
  MOVE_DIMENSION,
  FETCH_DATA,
  SCROLL,
  CHANGE_SORT_ORDER
} from "../constants";

export default (
  state = {
    selectedCellStart: { rowIndex: null, columnIndex: null },
    selectedCellEnd: { rowIndex: null, columnIndex: null },
    scrollToRow: { index: 0, direction: 1 },
    scrollToColumn: { index: 0, direction: 1 }
  },
  action
) => {
  const { type, selectedRange, scrollToRow, scrollToColumn, axis } = action;
  switch (type) {
    case FETCH_DATA:
    case MOVE_DIMENSION:
      return {
        ...state,
        selectedCellStart: { rowIndex: null, columnIndex: null },
        selectedCellEnd: { rowIndex: null, columnIndex: null }
      };
    case SELECT_RANGE:
      return {
        ...state,
        ...selectedRange
      };
    case SCROLL:
      const nextState = { ...state };
      if (scrollToRow !== null) {
        nextState.scrollToRow = scrollToRow;
      }
      if (scrollToColumn !== null) {
        nextState.scrollToColumn = scrollToColumn;
      }
      return nextState;
    // just to force the refresh
    case CHANGE_SORT_ORDER:
      const newState = { ...state };
      if (axis === "rows") {
        newState.scrollToRow.refreshLeaves = !newState.scrollToRow
          .refreshLeaves;
      } else if (axis === "columns") {
        newState.scrollToColumn.refreshLeaves = !newState.scrollToColumn
          .refreshLeaves;
      }
      return newState;
    default:
      return state;
  }
};
