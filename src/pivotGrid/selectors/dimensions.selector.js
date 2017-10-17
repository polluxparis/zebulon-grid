///////////////////////////////////////////////////////////////////
//  dimensions and measures (+ assignations in rows or columns) selectors
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";

import { ROOT_ID, TOTAL_ID, MEASURE_ID, AxisType } from "../constants";
const getAxisDimensions = (axis, dimensions) => {
  let prevDimension = { id: ROOT_ID };
  return axis.map(id => {
    const dimension = dimensions[id];
    if (
      dimension.attributeParents.includes(prevDimension.id) ||
      (dimension.attributeParents.includes(prevDimension.isAttributeOf) &&
        prevDimension.isAttribute)
    ) {
      dimension.isAttribute = true;
      dimension.isAttributeOf = prevDimension.isAttributeOf || prevDimension.id;
      // dimension.isCollapsed = prevDimension.isCollapsed;
      prevDimension.hasAttribute = !prevDimension.isAttribute;
    } else {
      // dimension.isCollapsed = collapses[dimension.id];
      dimension.isAttribute = false;
    }
    prevDimension = dimension;
    return dimension;
  });
  // .filter(dimension => {
  //   return !(dimension.isCollapsed && dimension.isAttribute);
  // })
  // ;
};
const getVisibleDimensions = (dimensions, collapses, hasMeasures) => {
  let prevId;
  const dims = dimensions.map(dimension => {
    if (dimension.isAttribute) {
      dimension.isVisible = !collapses[prevId];
    } else {
      dimension.isVisible = true;
      prevId = dimension.id;
    }
    return dimension;
  });
  if (hasMeasures) {
    dims.push({
      id: MEASURE_ID,
      isVisible: true,
      sort: {},
      caption: "Measures"
    });
  }
  if (!dims.length) {
    dims.push({ id: TOTAL_ID, isVisible: true, sort: {} });
  }
  return dims;
};

export const rowDimensionsSelector = createSelector(
  [state => state.axis.rows, state => state.dimensions],
  getAxisDimensions
);

export const columnDimensionsSelector = createSelector(
  [state => state.axis.columns, state => state.dimensions],
  getAxisDimensions
);
export const rowVisibleDimensionsSelector = createSelector(
  [
    rowDimensionsSelector,
    state => state.collapses.dimensions,
    state => state.config.measureHeadersAxis === "rows"
  ],
  getVisibleDimensions
);

export const columnVisibleDimensionsSelector = createSelector(
  [
    columnDimensionsSelector,
    state => state.collapses.dimensions,
    state => state.config.measureHeadersAxis === "columns"
  ],
  getVisibleDimensions
);

export const availableDimensionsSelector = createSelector(
  [state => state.axis.dimensions, state => state.dimensions],
  (dimensionAxis, dimensions) => dimensionAxis.map(id => dimensions[id])
);
export const dimensionsSelector = createSelector(
  [state => state.dimensions],
  dimensions => Object.keys(dimensions).map(id => dimensions[id])
);
export const dimensionsWithAxisSelector = createSelector(
  [
    rowDimensionsSelector,
    columnDimensionsSelector,
    availableDimensionsSelector
  ],
  (row, column, available) =>
    row
      .map((dimension, index) => {
        dimension.depth = index;
        dimension.axis = AxisType.ROWS;
        return dimension;
      })
      .concat(
        column.map((dimension, index) => {
          dimension.depth = index;
          dimension.axis = AxisType.COLUMNS;
          return dimension;
        })
      )
      .concat(available)
      .filter(dimension => dimension.id !== MEASURE_ID)
);
const measuresSelector = state => state.measures;

export const activatedMeasuresSelector = createSelector(
  [measuresSelector, state => state.axis.measures],
  (measures, activatedMeasures) =>
    activatedMeasures
      .map(id => measures[id])
      .reduce((acc, mea) => ({ ...acc, [mea.id]: mea }), {})
);
export const availableMeasuresSelector = createSelector(
  [measuresSelector, state => state.axis.measures],
  (measures, activatedMeasures) =>
    Object.keys(measures)
      .map(id => measures[id])
      .filter(measure => !activatedMeasures.includes(measure.id))
);
