import { SET_FIELDS, CHANGE_SORT_ORDER } from '../constants';

export default (state = {}, action) => {
  const { type, fields, fieldId } = action;
  let sort;
  switch (type) {
    case SET_FIELDS:
      return fields.reduce((acc, field) => ({ ...acc, [field.id]: field }), {});
    case CHANGE_SORT_ORDER:
      sort = state[fieldId].sort;
      if (sort.order === 'asc') {
        sort.order = 'desc';
      } else {
        sort.order = 'asc';
      }
      return { ...state, [fieldId]: { ...state[fieldId], sort } };
    default:
      return state;
  }
};
