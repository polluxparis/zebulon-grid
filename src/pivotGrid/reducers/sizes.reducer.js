import { UPDATE_CELL_SIZE } from '../constants';

export default (
  state = {
    heights: { '11-/-0': 20 },
    widths: { 'titi 0-/-qty': 50 }
  },
  action
) => {
  const { type, axis, direction, size, id } = action;
  switch (type) {
    case UPDATE_CELL_SIZE:
      return {
        ...state,
        [direction]: {
          ...state[direction],
          // [axis]: {
          //   ...state[direction][axis],
          [id]: size
          // }
        }
      };
    default:
      return state;
  }
};
