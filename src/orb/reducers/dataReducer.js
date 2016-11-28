import { PUSH_DATA } from '../constants';

export default (state = [], action) => {
  switch (action.type) {
    case PUSH_DATA:
      return [...state, ...action.payload];
    default:
      return state;
  }
};
