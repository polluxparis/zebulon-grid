import { createSelector } from 'reselect';

// import { scrollbarSize } from '../utils/domHelpers';
// import { getLeaves } from '../utils/generic';
import { AxisType, toAxisType } from '../Axis';
// import { MEASURE_ID, TOTAL_ID } from '../constants';
//import { getColumnLeaves, rowLeavesSelector } from './axis.selector';
// import {
//   columnDimensionsSelector,
//   rowDimensionsSelector
// } from './dimensions.selector';



export const getIsCollapsedColumnByKeySelector = createSelector(
  [state => state.collapses.columns,
  areCollapsed => key => {
    return areCollapsed[key] || false;
  }
);
export const getIsCollapsedRowByKeySelector = createSelector(
  [state => state.collapses.rows,
  areCollapsed => key => {
    return areCollapsed[key] || false;
  }
);
