import { SELECT_RANGE, MOVE_DIMENSION } from '../constants';

export default (
  state = {
    selectedCellStart: null,
    selectedCellEnd: null,
    focusedCell: null
  },
  action
) => {
  const { type, selectedRange } = action;
  console.log(['selector reducer', type]);
  switch (type) {
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
