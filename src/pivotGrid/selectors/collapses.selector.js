import { createSelector } from 'reselect';

import { AxisType, toAxisType } from '../constants';

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
