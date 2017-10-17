import { SET_MEASURES, FETCH_DATA } from "../constants";

export default (state = {}, action) => {
  const { type, measures, id } = action;
  switch (type) {
    // case FETCH_DATA:
    //   return {};
    case SET_MEASURES:
      return measures.reduce(
        (acc, measure) => ({ ...acc, [measure.id]: measure }),
        {}
      );
    default:
      return state;
  }
};
