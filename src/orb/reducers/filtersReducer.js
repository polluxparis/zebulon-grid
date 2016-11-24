import { SET_CONFIG } from '../actions';

export default (state = {}, action) => {
  const { type } = action;
  switch (type) {
    case SET_CONFIG:
      return state;
    default:
      return state;
  }
};
