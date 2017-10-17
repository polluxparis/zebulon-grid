import {
  SET_DIMENSIONS,
  // CHANGE_SORT_ORDER,
  FETCH_DATA
  // TOTAL_ID,
  // MEASURE_ID
} from "../constants";

export default (state = {}, action) => {
  const { type, dimensions } = action;
  // let sort;
  switch (type) {
    // case FETCH_DATA:
    //   return {};
    case SET_DIMENSIONS:
      return dimensions.reduce(
        (acc, dimension) => ({ ...acc, [dimension.id]: dimension }),
        {}
      );
    // case CHANGE_SORT_ORDER:
    // if (key === TOTAL_ID || key === MEASURE_ID) {
    //   return state;
    // }
    // let dimension = state[key];
    // sort = dimension.sort;
    // sort.sortedBy = null;
    // if (sort.direction === "desc") {
    //   sort.direction = "asc";
    // } else {
    //   sort.direction = "desc";
    // }
    // const dimensionsSort = { [key]: { ...dimension, sort } };
    // if (dimension.isAttribute) {
    //   dimensionsSort[dimension.isAttributeOf] = {
    //     ...state[dimension.isAttributeOf],
    //     sort: {
    //       ...state[dimension.isAttributeOf].sort,
    //       sortedBy: key
    //     }
    //   };
    // }

    // return {
    //   ...state,
    //   ...dimensionsSort
    // };
    // if (key !== TOTAL_ID && key !== MEASURE_ID) {
    //   const sort = state[key].sort;
    //   if (sort.direction === "desc") {
    //     sort.direction = "asc";
    //   } else {
    //     sort.direction = "desc";
    //   }
    // }
    // return state;
    default:
      return state;
  }
};
