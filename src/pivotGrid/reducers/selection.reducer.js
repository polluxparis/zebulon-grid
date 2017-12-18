import { SELECT_RANGE, MOVE_DIMENSION, FETCH_DATA, SCROLL } from "../constants";

export default (
  state = {
    start: { rows: null, columns: null },
    end: { rows: null, columns: null },
    scrollToRow: { index: 0, direction: 1 },
    scrollToColumn: { index: 0, direction: 1 }
  },
  action
) => {
  const { type, selectedRange, scrollToRow, scrollToColumn } = action;
  switch (type) {
    case FETCH_DATA:
    case MOVE_DIMENSION:
      return {
        ...state,
        start: { rows: null, columns: null },
        end: { rows: null, columns: null }
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
    default:
      return state;
  }
};
