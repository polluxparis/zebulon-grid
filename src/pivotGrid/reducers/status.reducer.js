import {
  FETCH_SUCCESS,
  PUSH_DATA,
  FETCH_DATA,
  FETCH_FAILURE,
  CHANGE_SORT_ORDER
} from "../constants";

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
    case PUSH_DATA:
      return { ...state, loading: false, loaded: true, error: undefined };
    // just to force the refresh
    case CHANGE_SORT_ORDER:
      const newState = { ...state };
      newState.toRefreshLeaves[action.axis] = !newState.toRefreshLeaves[
        action.axis
      ];
      return newState;
    default:
      return state;
  }
};
