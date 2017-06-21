import { createSelector } from "reselect";
import { Axis, AxisType, toAxisType } from "../Axis";
import AxisUi from "../AxisUi";
import { getFilteredData } from "./data.selector";
import {
  getRowDimensions,
  getColumnDimensions,
  getActivatedMeasures
} from "./dimensions.selector";

export const getRowAxis = createSelector(
  [getRowDimensions, getFilteredData],
  (rowDimensions, filteredData) =>
    new Axis(AxisType.ROWS, rowDimensions, filteredData)
);

export const getColumnAxis = createSelector(
  [getColumnDimensions, getFilteredData],
  (columnDimensions, filteredData) =>
    new Axis(AxisType.COLUMNS, columnDimensions, filteredData)
);

const getActivatedMeasuresCount = createSelector(
  [getActivatedMeasures],
  measures => measures.length
);

const getAxisActivatedMeasures = axisType =>
  createSelector(
    [getActivatedMeasures, state => state.config.dataHeadersLocation],
    (measures, dataHeadersLocation) => {
      if (toAxisType(dataHeadersLocation) === axisType) {
        return measures;
      }
      return null;
    }
  );

export const getRowUiAxis = createSelector(
  [
    getRowAxis,
    getAxisActivatedMeasures(AxisType.ROWS),
    state => state.axis.columns
  ],
  (rowAxis, activatedMeasures, crossDimensionsCode) =>
    new AxisUi(
      rowAxis,
      {
        activatedMeasures,
        activatedMeasuresCount: activatedMeasures ? activatedMeasures.length : 0
      },
      crossDimensionsCode
    )
);

export const getColumnUiAxis = createSelector(
  [
    getColumnAxis,
    getAxisActivatedMeasures(AxisType.COLUMNS),
    state => state.axis.rows
  ],
  (columnAxis, activatedMeasures, crossDimensionsCode) =>
    new AxisUi(
      columnAxis,
      {
        activatedMeasures,
        activatedMeasuresCount: activatedMeasures ? activatedMeasures.length : 0
      },
      crossDimensionsCode
    )
);

export const getLayout = createSelector(
  [
    getRowAxis,
    getColumnAxis,
    getRowUiAxis,
    getColumnUiAxis,
    getActivatedMeasuresCount,
    state => state.config.dataHeadersLocation
  ],
  (
    rowAxis,
    columnAxis,
    rowsUi,
    columnsUi,
    activatedMeasuresCount,
    dataHeadersLocation
  ) => {
    const rowHorizontalCount =
      (rowAxis.dimensions.length || 1) +
      (dataHeadersLocation === "rows" && activatedMeasuresCount >= 1 ? 1 : 0);
    const rowVerticalCount = rowsUi.headers.length;
    const columnHorizontalCount = columnsUi.headers.length;
    const columnVerticalCount =
      (columnAxis.dimensions.length || 1) +
      (dataHeadersLocation === "columns" && activatedMeasuresCount >= 1
        ? 1
        : 0);
    return {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount
    };
  }
);
