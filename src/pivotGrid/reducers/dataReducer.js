import { PUSH_DATA, SET_DATA } from '../constants';

export default (state = [], action) => {
  switch (action.type) {
    case PUSH_DATA:
      return [...state, ...action.payload];
    case SET_DATA:
      return action.payload;
    default:
      return state;
  }
};
