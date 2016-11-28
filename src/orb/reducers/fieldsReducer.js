import { SET_CONFIG, CHANGE_SORT_ORDER } from '../actions';

export default (state = {}, action) => {
  const { type, config, field } = action;
  let sort;
  switch (type) {
    case SET_CONFIG:
      return config.allFields
        .reduce((acc, field) => ({ ...acc, [field.id]: field }), {});
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
