import { EXPAND_COLLAPSE, EXPAND_COLLAPSE_ATTRIBUTE } from '../constants';

export default (
  // state = { rows: { '1': true }, columns: { 'titi 1': true }, dimensions: {} },
  state = { rows: {}, columns: {}, dimensions: {} },
  action
) => {
  const { type, axis, key } = action;
  switch (type) {
    case EXPAND_COLLAPSE:
      return {
        ...state,
        [axis]: {
          ...state[axis],
          // [axis]: {
          //   ...state[direction][axis],
          [key]: !state[axis][key]
          // }
        }
      };
    default:
      return state;
  }
};
