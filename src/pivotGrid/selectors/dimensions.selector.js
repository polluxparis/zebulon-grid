import { createSelector } from 'reselect';

import { ROOT_ID } from '../constants';
const getAxisDimensions = (axis, dimensions, collapses) => {
  let prevDimension = { id: ROOT_ID };
  return axis
    .map(id => {
      const dimension = dimensions[id];
      if (
        prevDimension.id === dimension.isAttributeOf ||
        (prevDimension.isAttributeOf === dimension.isAttributeOf &&
          prevDimension.isAttribute)
      ) {
        dimension.isAttribute = true;
        dimension.isCollapsed = prevDimension.isCollapsed;
      } else {
        dimension.isCollapsed = collapses[dimension.id];
        dimension.isAttribute = false;
      }
      prevDimension = dimension;
      return dimension;
    })
    .filter(dimension => {
      return !(dimension.isCollapsed && dimension.isAttribute);
    });
};

export const rowDimensionsSelector = createSelector(
  [
    state => state.axis.rows,
    state => state.dimensions,
    state => state.collapses.dimensions
  ],
  getAxisDimensions
);

export const columnDimensionsSelector = createSelector(
  [
    state => state.axis.columns,
    state => state.dimensions,
    state => state.collapses.dimensions
  ],
  getAxisDimensions
);

export const availableDimensionsSelector = createSelector(
  [state => state.axis.dimensions, state => state.dimensions],
  (dimensionAxis, dimensions) => dimensionAxis.map(id => dimensions[id])
);
export const dimensionsSelector = createSelector(
  [state => state.dimensions],
  dimensions => Object.keys(dimensions).map(id => dimensions[id])
);
const measuresSelector = state => state.measures;

export const activatedMeasuresSelector = createSelector(
  [measuresSelector],
  measures =>
    Object.keys(measures)
      .map(id => measures[id])
      .filter(measure => measure.activated)
      .reduce((acc, mea) => ({ ...acc, [mea.id]: mea }), {})
);
export const availableMeasuresSelector = createSelector(
  [measuresSelector],
  measures =>
    Object.keys(measures)
      .map(id => measures[id])
      .filter(measure => !measure.activated)
);
