import { SET_CONFIG_PROPERTY, ADD_FILTER, DELETE_FILTER } from "../constants";

export default (state = {}, action) => {
  const { type, dimension, filter } = action;
  switch (type) {
    // Eventually users will be able to define filters in config
    case SET_CONFIG_PROPERTY:
      return state;
    case ADD_FILTER:
      return { ...state, [dimension]: filter };
    case DELETE_FILTER: {
      /* eslint-disable no-unused-vars */
      const { [dimension]: deleted, ...newState } = state;
      /* eslint-enable */
      return newState;
    }
    default:
      return state;
  }
};
