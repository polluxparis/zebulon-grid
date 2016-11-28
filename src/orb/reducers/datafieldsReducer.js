import { SET_CONFIG, TOGGLE_DATAFIELD } from '../actions';

export default (state = {}, action) => {
  const { type, config, id } = action;
  let datafield;
  switch (type) {
    case SET_CONFIG:
      return config.datafields
        .reduce((acc, field) => ({ ...acc, [field.id]: field }), {});
    case TOGGLE_DATAFIELD:
      datafield = state[id];
      return { ...state, [id]: { ...datafield, activated: !datafield.activated } };
    default:
      return state;
  }
};
