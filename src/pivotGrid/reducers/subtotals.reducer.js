import { TOGGLE_SUBTOTAL } from "../constants";

export default (state = {}, action) => {
  switch (action.type) {
    case TOGGLE_SUBTOTAL:
      return {
        ...state,
        [action.dimensionId]: !state[action.dimensionId]
      };
    default:
      return state;
  }
};
