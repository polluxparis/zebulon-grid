import { UPDATE_CELL_SIZE, SET_SIZES } from "../constants";

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
        }
      };
    case SET_SIZES:
      return {
        ...state,
        ...sizes
      };
    default:
      return state;
  }
};
