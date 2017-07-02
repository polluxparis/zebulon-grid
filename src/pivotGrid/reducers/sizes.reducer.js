import { UPDATE_CELL_SIZE } from '../constants';

export default (
  state = {
    heights: {},
    widths: {}
  },
  action
) => {
  const { type, direction, sizes } = action;
  switch (type) {
    case UPDATE_CELL_SIZE:
      return {
        ...state,
        [direction]: {
          ...state[direction],
          ...sizes
          // }
        }
      };
    default:
      return state;
  }
};
