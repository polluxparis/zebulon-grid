import { SET_DATAFIELDS, TOGGLE_DATAFIELD } from '../constants';

export default (state = {}, action) => {
  const { type, datafields, id } = action;
  let datafield;
  switch (type) {
    case SET_DATAFIELDS:
      return datafields.reduce(
        (acc, field) => ({ ...acc, [field.id]: field }),
        {},
      );
    case TOGGLE_DATAFIELD:
      datafield = state[id];
      return {
        ...state,
        [id]: { ...datafield, activated: !datafield.activated },
      };
    default:
      return state;
  }
};
