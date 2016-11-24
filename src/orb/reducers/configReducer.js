import { SET_CONFIG_PROPERTY } from '../actions';

export default (state = {}, action) => {
  const { type, property, value } = action;
  switch (type) {
    case SET_CONFIG_PROPERTY:
      return { ...state, [property]: value };
    default:
      return state;
  }
};
