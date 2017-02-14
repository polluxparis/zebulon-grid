import { SET_FIELDS, CHANGE_SORT_ORDER } from '../constants';

export default (state = {}, action) => {
  const { type, fields, field } = action;
  let sort;
  switch (type) {
    case SET_FIELDS:
      return fields.reduce((acc, field) => ({ ...acc, [field.id]: field }), {});
    case CHANGE_SORT_ORDER:
      sort = state[field].sort;
      if (sort.order === 'asc') {
        sort.order = 'desc';
      } else {
        sort.order = 'asc';
      }
      return { ...state, [field]: { ...state[field], sort } };
    default:
      return state;
  }
};
