import { MOVE_FIELD } from '../constants';

export default (state = { rows: [], columns: [], fields: [] }, action) => {
  const { type, id, position, oldAxis, newAxis } = action;
  let newAxisValue;
  let oldAxisValue;
  switch (type) {
    case MOVE_FIELD:
      newAxisValue = [...state[newAxis].slice(0, position), id, ...state[newAxis].slice(position)];
      oldAxisValue = state[oldAxis].filter(field => field !== id);
      return { ...state, [newAxis]: newAxisValue, [oldAxis]: oldAxisValue };
    default:
      return state;
  }
};
