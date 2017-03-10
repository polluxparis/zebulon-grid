import { createSelector } from 'reselect';
import { Axis, AxisType } from '../Axis';
import AxisUi from '../AxisUi';
import { getFilteredData } from './data.selector';
import {
  getRowFields,
  getColumnFields,
  getActivatedDataFields
} from './fields.selector';

export const getRowAxis = createSelector(
  [getRowFields, getFilteredData],
  (rowFields, filteredData) => new Axis(AxisType.ROWS, rowFields, filteredData)
);

export const getColumnAxis = createSelector(
  [getColumnFields, getFilteredData],
  (columnFields, filteredData) =>
    new Axis(AxisType.COLUMNS, columnFields, filteredData)
);

const getActivatedDataFieldsCount = createSelector(
  [getActivatedDataFields],
  datafields => datafields.length
);

export const getRowUiAxis = createSelector(
  [
    getRowAxis,
    getActivatedDataFields,
    getActivatedDataFieldsCount,
    state => state.config.dataHeadersLocation,
    state => state.axis.columns
  ],
  (
    rowAxis,
    activatedDataFields,
    activatedDataFieldsCount,
    dataHeadersLocation,
    crossFieldsCode
  ) =>
    new AxisUi(
      rowAxis,
      { activatedDataFields, activatedDataFieldsCount, dataHeadersLocation },
      crossFieldsCode
    )
);

export const getColumnUiAxis = createSelector(
  [
    getColumnAxis,
    getActivatedDataFields,
    state => state.config.dataHeadersLocation,
    state => state.axis.rows
  ],
  (columnAxis, activatedDataFields, dataHeadersLocation, crossFieldsCode) =>
    new AxisUi(
      columnAxis,
      {
        activatedDataFields,
        activatedDataFieldsCount: activatedDataFields.length,
        dataHeadersLocation
      },
      crossFieldsCode
    )
);

export const getLayout = createSelector(
  [
    getRowAxis,
    getColumnAxis,
    getRowUiAxis,
    getColumnUiAxis,
    getActivatedDataFieldsCount,
    state => state.config.dataHeadersLocation
  ],
  (rowAxis, columnAxis, rowsUi, columnsUi, activatedDataFieldsCount, dataHeadersLocation) => {
    const rowHorizontalCount = (rowAxis.fields.length || 1) +
      (dataHeadersLocation === 'rows' && activatedDataFieldsCount >= 1 ? 1 : 0);
    const rowVerticalCount = rowsUi.headers.length;
    const columnHorizontalCount = columnsUi.headers.length;
    const columnVerticalCount = (columnAxis.fields.length || 1) +
      (dataHeadersLocation === 'columns' && activatedDataFieldsCount >= 1
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
