import { SET_DIMENSIONS } from "../constants";

export default (state = {}, action) => {
  const { type, dimensions } = action;
  // let sort;
  switch (type) {
    case SET_DIMENSIONS:
      return dimensions.reduce(
        (acc, dimension) => ({ ...acc, [dimension.id]: dimension }),
        {}
      );
    default:
      return state;
  }
};
