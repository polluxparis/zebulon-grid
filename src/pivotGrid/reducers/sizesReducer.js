import { UPDATE_CELL_SIZE } from '../constants';

export default (
  state = { rows: { leafs: {}, dimensions: {} }, columns: { leafs: {}, dimensions: {} } },
  action,
) => {
  const { type, axis, direction, size, id } = action;
  switch (type) {
    case UPDATE_CELL_SIZE:
      return {
        ...state,
        [axis]: {
          ...state[axis],
          [direction]: {
            ...state[axis][direction],
            [id]: size,
          },
        },
      };
    default:
      return state;
  }
};
