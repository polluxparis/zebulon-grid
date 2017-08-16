import {
  FETCH_SUCCESS,
  PUSH_DATA,
  FETCH_DATA,
  FETCH_FAILURE
} from '../constants';

export default (
  state = { loading: false, loaded: false, error: undefined },
  action
) => {
  const { type, error } = action;
  switch (type) {
    case FETCH_DATA:
      return { loading: true, loaded: false, error: undefined };
    case FETCH_FAILURE:
      return { loading: false, loaded: false, error };
    case FETCH_SUCCESS:
    case PUSH_DATA:
      return {
        loading: false,
        loaded: true,
        error: undefined
      };
    default:
      return state;
  }
};
