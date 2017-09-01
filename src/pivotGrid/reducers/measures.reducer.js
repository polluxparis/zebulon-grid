import { SET_MEASURES } from "../constants";

export default (state = {}, action) => {
  const { type, measures, id } = action;
  switch (type) {
    case SET_MEASURES:
      return measures.reduce(
        (acc, measure) => ({ ...acc, [measure.id]: measure }),
        {}
      );
    default:
      return state;
  }
};
