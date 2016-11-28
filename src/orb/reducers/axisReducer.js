import { ADD_FIELD, REMOVE_FIELD } from '../actions';

export default (state = { rows: [], columns: [], fields: [] }, action) => {
  const { type, id, position, axis } = action;
  let newAxis;
  switch (type) {
    case ADD_FIELD:
      newAxis = [...state[axis].slice(0, position), id, ...state[axis].slice(position)];
      return { ...state, [axis]: newAxis };
    case REMOVE_FIELD:
      newAxis = state[axis].filter(field => field !== id);
      return { ...state, [axis]: newAxis };
    default:
      return state;
  }
};
