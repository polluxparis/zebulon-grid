import { SET_DIMENSIONS, CHANGE_SORT_ORDER } from '../constants';

export default (state = {}, action) => {
  const { type, dimensions, dimensionId } = action;
  let sort;
  switch (type) {
    case SET_DIMENSIONS:
      return dimensions.reduce(
        (acc, dimension) => ({ ...acc, [dimension.id]: dimension }),
        {}
      );
    case CHANGE_SORT_ORDER:
      sort = state[dimensionId].sort;
      if (sort.direction === 'asc') {
        sort.direction = 'desc';
      } else {
        sort.direction = 'asc';
      }
      return { ...state, [dimensionId]: { ...state[dimensionId], sort } };
    default:
      return state;
  }
};
