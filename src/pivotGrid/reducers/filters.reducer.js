import { SET_CONFIG, ADD_FILTER, DELETE_FILTER } from '../constants';

export default (state = {}, action) => {
  const { type, field, filter } = action;
  switch (type) {
    // Eventually users will be able to define filters in config
    case SET_CONFIG:
      return state;
    case ADD_FILTER:
      return { ...state, [field]: filter };
    case DELETE_FILTER: {
      const { [field]: deleted, ...newState } = state;
      return newState;
    }
    default:
      return state;
  }
};
