import { SET_DIMENSIONS, LOADING_CONFIG } from "../constants";

export default (state = {}, action) => {
  const { type, dimensions } = action;
  // let sort;
  switch (type) {
    case LOADING_CONFIG:
      if (action.loading) {
        return {};
      } else {
        return state;
      }
    case SET_DIMENSIONS:
      return dimensions.reduce(
        (acc, dimension) => ({ ...acc, [dimension.id]: dimension }),
        {}
      );
    default:
      return state;
  }
};
