export const MEASURE_ID = "__measures__";
export const TOTAL_ID = "__total__";

export const KEY_SEPARATOR = "-/-";
export const AXIS_SEPARATOR = "-//-";

export const ROOT_ID = "__ROOT_ID__";
export const EMPTY_ID = "__EMPTY_ID__";
export const ScrollbarSize = 12;
// Headers
export const HeaderType = {
  MEASURE: 1, // measure header
  DIMENSION: 2, // dimension value (leaf)
  DIMENSION_HEADER: 3, // dimension header
  GRAND_TOTAL: 4,
  SUB_TOTAL: 5
};

// Axis
export const AxisType = {
  COLUMNS: 1,
  ROWS: 2,
  MEASURE: 3,
  DIMENSION: 4,
  DATACELL: 5
};

export function toAxis(axisType) {
  switch (axisType) {
    case AxisType.COLUMNS:
      return "columns";
    case AxisType.ROWS:
      return "rows";
    case AxisType.MEASURES:
      return "measures";
    case AxisType.DIMENSION:
      return "dimensions";
    default:
      return "__AXIS_TYPE_UNKNOWN__";
  }
}

export function toAxisType(axis) {
  return AxisType[axis.toUpperCase()];
}

// ACTIONS

export const SET_DIMENSIONS = "PIVOTGRID_SET_DIMENSIONS";
export const SET_MEASURES = "PIVOTGRID_SET_MEASURES";
export const SET_CONFIG_PROPERTY = "PIVOTGRID_SET_CONFIG_PROPERTY";
export const TOGGLE_MEASURE = "PIVOTGRID_TOGGLE_MEASURE";
export const MOVE_MEASURE = "PIVOTGRID_MOVE_MEASURE";
export const MOVE_DIMENSION = "PIVOTGRID_MOVE_DIMENSION";
export const SET_AXIS = "PIVOTGRID_SET_AXIS";

export const FETCH_DATA = "PIVOTGRID_FETCH_DATA";
export const FETCH_FAILURE = "PIVOTGRID_FETCH_FAILURE";
export const PUSH_DATA = "PIVOTGRID_PUSH_DATA";
export const FETCH_SUCCESS = "PIVOTGRID_FETCH_SUCCESS";
export const LOADING_CONFIG = "PIVOTGRID_LOADING_CONFIG";

export const CHANGE_SORT_ORDER = "PIVOTGRID_CHANGE_SORT_ORDER";

export const ADD_FILTER = "PIVOTGRID_ADD_FILTER";
export const DELETE_FILTER = "PIVOTGRID_DELETE_FILTER";

export const UPDATE_CELL_SIZE = "PIVOTGRID_UPDATE_CELL_SIZE";
export const SET_SIZES = "PIVOTGRID_SET_SIZES";

export const ZOOM_IN = "PIVOTGRID_ZOOM_IN";
export const ZOOM_OUT = "PIVOTGRID_ZOOM_OUT";

export const EXPAND_COLLAPSE = "PIVOTGRID_EXPAND_COLLAPSE";
export const EXPAND_COLLAPSE_ALL = "PIVOTGRID_EXPAND_COLLAPSE_ALL";
export const SET_COLLAPSES = "PIVOTGRID_SET_COLLAPSES";

export const SELECT_RANGE = "PIVOTGRID_SELECT_RANGE";
export const SCROLL = "PIVOTGRID_SCROLL";
export const TOGGLE_SUBTOTAL = "PIVOTGRID_TOGGLE_SUBTOTAL";
