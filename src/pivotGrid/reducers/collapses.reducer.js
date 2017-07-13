import { EXPAND_COLLAPSE } from '../constants';

export default (state = { rows: {}, columns: {}, dimensions: {} }, action) => {
  const { type, axis, key } = action;
  switch (type) {
    case EXPAND_COLLAPSE:
      return {
        ...state,
        [axis]: {
          ...state[axis],
          [key]: !state[axis][key]
        }
      };
    default:
      return state;
  }
};
