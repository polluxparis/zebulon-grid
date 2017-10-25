import { SET_MEASURES, LOADING_CONFIG } from "../constants";

export default (state = {}, action) => {
  const { type, measures } = action;
  switch (type) {
    case LOADING_CONFIG:
      if (action.loading) {
        return {};
      } else {
        return state;
      }
    case SET_MEASURES:
      return measures.reduce(
        (acc, measure) => ({ ...acc, [measure.id]: measure }),
        {}
      );
    default:
      return state;
  }
};
