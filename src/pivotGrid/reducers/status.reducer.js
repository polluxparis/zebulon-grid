import {
  FETCH_SUCCESS,
  PUSH_DATA,
  FETCH_DATA,
  FETCH_FAILURE,
  LOADING_CONFIG,
  CHANGE_SORT_ORDER
} from "../constants";

export default (
  state = {
    loading: false,
    loaded: false,
    error: undefined,
    loadingConfig: false,
    pushed: false
  },
  action
) => {
  const { type, error } = action;
  switch (type) {
    case FETCH_DATA:
      return {
        ...state,
        loading: true,
        loaded: false,
        error: undefined,
        pushed: false
      };
    case FETCH_FAILURE:
      return { ...state, loading: false, loaded: false, error };
    case FETCH_SUCCESS:
      return {
        ...state,
        loading: false,
        loaded: true,
        error: undefined
      };
    case PUSH_DATA:
      return {
        ...state,
        pushed: true
      };
    case LOADING_CONFIG:
      return { ...state, loadingConfig: action.loading };
    // just to force the refresh
    case CHANGE_SORT_ORDER:
      return { ...state, refreshDisplay: !state.refreshDisplay };
    default:
      return state;
  }
};
