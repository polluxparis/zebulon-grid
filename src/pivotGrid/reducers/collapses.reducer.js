import {
  EXPAND_COLLAPSE,
  EXPAND_COLLAPSE_ALL,
  SET_COLLAPSES,
  SET_MEASURES,
  MOVE_DIMENSION,
  TOGGLE_MEASURE,
  ADD_FILTER,
  DELETE_FILTER,
  TOGGLE_SUBTOTAL
} from "../constants";

/*
Collapses are needed to compute number of visible rows(or columns), displays... on the whole data set.
The best moment to compute required data is during the calculation of headers trees (additional cost close to 0)
when the data set is very large, it may take significant time to recompute the axis trees...,
In consequence, for performance purpose, the collapses calculations are only when executed the axis trees have to be.
The intermediate change in the collapse description are managed by mutating the objects (headers).
to avoid that the selector refired in this case, collapses are managed at 2 levels, the config level (used by the axis tree selector) and the standard level
(used for the rest).
When the axis tree must be recalculated, the config level must be set as the standard level.
*/
export default (
  state = {
    rows: {},
    columns: {},
    dimensions: {},
    configurationRows: {},
    nVisibleRows: 0,
    configurationColumns: {},
    nVisibleColumns: 0
  },
  action
) => {
  const { type, axis, key, keys, collapses, n } = action;
  let newState;
  switch (type) {
    case EXPAND_COLLAPSE:
      newState = {
        ...state,
        [axis]: {
          ...state[axis],
          [key]: !state[axis][key]
        }
      };
      if (axis === "rows") {
        newState.nVisibleRows += n;
      } else if (axis === "columns") {
        newState.nVisibleColumns += n;
      }
      return newState;
    case EXPAND_COLLAPSE_ALL:
      newState = {
        ...state,
        [axis]: {
          ...state[axis],
          ...keys
        }
      };
      return newState;
    // apply actual collapses to confix collapses as header Trees will be recalculate
    // case DELETE_FILTER:
    // case ADD_FILTER:
    case SET_MEASURES:
    case MOVE_DIMENSION:
    case ADD_FILTER:
    case DELETE_FILTER:
    case TOGGLE_SUBTOTAL:
    case TOGGLE_MEASURE:
      const rows = { ...state.configurationRows, ...state.rows };
      const columns = { ...state.configurationColumns, ...state.columns };
      return {
        rows,
        columns,
        dimensions: state.dimensions,
        configurationRows: rows,
        nVisibleColumns: 0,
        nVisibleRows: 0,
        configurationColumns: columns
      };
    case SET_COLLAPSES:
      return {
        ...state,
        ...collapses
      };
    default:
      return state;
  }
};
