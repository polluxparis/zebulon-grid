import { FETCH_SUCCESS, FETCH_DATA, FETCH_FAILURE } from "../constants";

export default (
  state = {
    loading: false,
    loaded: false,
    error: undefined,
    toRefreshLeaves: { rows: false, columns: false }
  },
  action
) => {
  const { type, error } = action;
  switch (type) {
    case FETCH_DATA:
      return { ...state, loading: true, loaded: false, error: undefined };
    case FETCH_FAILURE:
      return { ...state, loading: false, loaded: false, error };
    case FETCH_SUCCESS:
      return { ...state, loading: false, loaded: false, error: undefined };
    default:
      return state;
  }
};
