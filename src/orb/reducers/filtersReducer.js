import { SET_CONFIG, ADD_FILTER, DELETE_FILTER } from '../constants';

export default (state = {}, action) => {
  const { type, field, filter } = action;
  switch (type) {
    case SET_CONFIG:
      return state;
    case ADD_FILTER:
      return { ...state, [field]: filter };
    case DELETE_FILTER:
      let { [field]: deleted, ...newState } = state;
      return newState;
    default:
      return state;
  }
};
