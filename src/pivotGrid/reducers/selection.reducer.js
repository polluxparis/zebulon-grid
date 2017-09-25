import { SELECT_RANGE, MOVE_DIMENSION, FETCH_DATA } from "../constants";

export default (
  state = {
    selectedCellStart: null,
    selectedCellEnd: null,
    focusedCell: null
  },
  action
) => {
  const { type, selectedRange } = action;
  switch (type) {
    case FETCH_DATA:
    case MOVE_DIMENSION:
      return {
        ...state,
        selectedCellStart: null,
        selectedCellEnd: null,
        focusedCell: null
      };
    case SELECT_RANGE:
      return {
        ...state,
        ...selectedRange
      };
    default:
      return state;
  }
};
