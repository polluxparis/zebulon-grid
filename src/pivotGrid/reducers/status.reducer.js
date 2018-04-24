import {
  FETCH_SUCCESS,
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
    loadingConfig: false
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
      // console.log(FETCH_SUCCESS, {
      //   loading: false,
      //   loaded: true,
      //   error: undefined
      // });
      return { ...state, loading: false, loaded: true, error: undefined };
    case LOADING_CONFIG:
      return { ...state, loadingConfig: action.loading };
    // just to force the refresh
    case CHANGE_SORT_ORDER:
      return { ...state, refreshDisplay: !state.refreshDisplay };
    default:
      return state;
  }
};
