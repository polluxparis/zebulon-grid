import { PUSH_DATA } from '../actions';

export default (state = [], action) => {
  switch (action.type) {
    case PUSH_DATA:
      return [...state, action.payload];
    default:
      return state;
  }
};
