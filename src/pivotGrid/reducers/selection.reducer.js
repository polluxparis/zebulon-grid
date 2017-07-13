import {
  EXPAND_COLLAPSE,
  EXPAND_COLLAPSE_ATTRIBUTE,
  SELECT_RANGE,
  MOVE_DIMENSION
} from '../constants';

export default (
  // state = { rows: { '1': true }, columns: { 'titi 1': true }, dimensions: {} },
  state = {
    selectedCellStart: null,
    selectedCellEnd: null,
    focusedCell: null
  },
  action
) => {
  const { type, selectedRange } = action;
  switch (type) {
    case MOVE_DIMENSION:
      return {
        ...state,
        selectedCellStart: null,
        selectedCellEnd: null,
        focusedCell: null
      };
      break;
    case SELECT_RANGE:
      return {
        ...state,
        ...selectedRange
      };
    default:
      return state;
  }
};
