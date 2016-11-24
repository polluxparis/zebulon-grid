import { SET_CONFIG } from '../actions';

export default (state = {}, action) => {
  const { type, config } = action;
  switch (type) {
    case SET_CONFIG:
      return config.allFields
        .reduce((acc, field) => ({ ...acc, [field.id]: field }), {});
    default:
      return state;
  }
};
