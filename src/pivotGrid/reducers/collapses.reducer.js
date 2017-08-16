import { EXPAND_COLLAPSE, EXPAND_COLLAPSE_ALL } from '../constants';

export default (state = { rows: {}, columns: {}, dimensions: {} }, action) => {
  const { type, axis, key, keys } = action;
  switch (type) {
    case EXPAND_COLLAPSE:
      return {
        ...state,
        [axis]: {
          ...state[axis],
          [key]: !state[axis][key]
        }
      };
    case EXPAND_COLLAPSE_ALL:
      return {
        ...state,
        [axis]: {
          ...state[axis],
          ...keys
        }
      };
    default:
      return state;
  }
};
