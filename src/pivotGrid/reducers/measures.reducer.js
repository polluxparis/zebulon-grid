import { SET_MEASURES, TOGGLE_MEASURE } from "../constants";

export default (state = {}, action) => {
  const { type, measures, id } = action;
  let measure;
  switch (type) {
    case SET_MEASURES:
      return measures.reduce(
        (acc, measure) => ({ ...acc, [measure.id]: measure }),
        {}
      );
    case TOGGLE_MEASURE:
      measure = state[id];
      return {
        ...state,
        [id]: { ...measure, activated: !measure.activated }
      };
    default:
      return state;
  }
};
