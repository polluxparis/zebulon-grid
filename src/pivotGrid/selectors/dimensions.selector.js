///////////////////////////////////////////////////////////////////
//  dimensions and measures (+ assignations in rows or columns) selectors
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";

import { ROOT_ID, TOTAL_ID, MEASURE_ID, AxisType, toAxis } from "../constants";
const getAxisDimensions = (axis, axisDimensions, dimensions) => {
  let prevDimension = { id: ROOT_ID };
  return axisDimensions.map((id, index) => {
    const dimension = dimensions[id];
    if (
      dimension.attributeParents.includes(prevDimension.id) ||
      (dimension.attributeParents.includes(prevDimension.isAttributeOf) &&
        prevDimension.isAttribute)
    ) {
      dimension.axis = axis;
      dimension.depth = index;
      dimension.isAttribute = true;
      dimension.isAttributeOf = prevDimension.isAttributeOf || prevDimension.id;
      prevDimension.hasAttribute = !prevDimension.isAttribute;
    } else {
      dimension.isAttribute = false;
    }
    prevDimension = dimension;
    dimension.orderBy = undefined;

    return dimension;
  });
};
const getVisibleDimensions = (
  axis,
  dimensions,
  collapses,
  subTotals,
  hasMeasures
) => {
  let prevId;
  const hasGrandTotal = subTotals[`${toAxis(axis)}${TOTAL_ID}`] || false;
  const dims = dimensions
    .map(dimension => {
      if (dimension.isAttribute) {
        dimension.isVisible = !collapses[prevId];
        dimension.hasSubTotal = null;
      } else {
        dimension.isVisible = true;
        dimension.hasSubTotal = subTotals[dimension.id] || false;
        dimension.hasGrandTotal = hasGrandTotal;
        prevId = dimension.id;
      }
      return dimension;
    })
    .map(dimension => {
      if (dimension.id === prevId) {
        dimension.hasSubTotal = null;
      }
      return dimension;
    });
  if (hasMeasures) {
    dims.push({
      id: MEASURE_ID,
      isVisible: true,
      sort: {},
      caption: "Measures",
      axis
    });
  }
  if (!dims.length) {
    dims.push({ id: TOTAL_ID, isVisible: true, sort: {} });
  }
  return dims;
};

export const rowDimensionsSelector = createSelector(
  [state => state.axis.rows, state => state.dimensions],
  (axisDimensions, dimensions) =>
    getAxisDimensions(AxisType.ROWS, axisDimensions, dimensions)
);

export const columnDimensionsSelector = createSelector(
  [state => state.axis.columns, state => state.dimensions],
  (axisDimensions, dimensions) =>
    getAxisDimensions(AxisType.COLUMNS, axisDimensions, dimensions)
);
export const rowVisibleDimensionsSelector = createSelector(
  [
    rowDimensionsSelector,
    state => state.collapses.dimensions,
    state => state.subtotals,
    state => state.axis.measuresAxis === "rows"
  ],
  (dimensions, collapses, subTotals, hasMeasures) =>
    getVisibleDimensions(
      AxisType.ROWS,
      dimensions,
      collapses,
      subTotals,
      hasMeasures
    )
);

export const columnVisibleDimensionsSelector = createSelector(
  [
    columnDimensionsSelector,
    state => state.collapses.dimensions,
    state => state.subtotals,
    state => state.axis.measuresAxis === "columns"
  ],
  (dimensions, collapses, subTotals, hasMeasures) =>
    getVisibleDimensions(
      AxisType.COLUMNS,
      dimensions,
      collapses,
      subTotals,
      hasMeasures
    )
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
      .concat(
        available.map(dimension => {
          dimension.depth = null;
          dimension.axis = null;
          return dimension;
        })
      )
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
