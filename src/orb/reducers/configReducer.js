import { SET_CONFIG } from '../actions';

export default (state = {}, action) => {
  switch (action.type) {
    case SET_CONFIG:
      return action.config;
    default:
      return state;
  }
};
