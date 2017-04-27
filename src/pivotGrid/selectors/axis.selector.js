import { createSelector } from 'reselect';
import { Axis, AxisType, toAxisType } from '../Axis';
import AxisUi from '../AxisUi';
import { getFilteredData } from './data.selector';
import {
  getRowFields,
  getColumnFields,
  getActivatedDatafields
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

const getActivatedDatafieldsCount = createSelector(
  [getActivatedDatafields],
  datafields => datafields.length
);

const getAxisActivatedDatafields = axisType =>
  createSelector(
    [getActivatedDatafields, state => state.config.dataHeadersLocation],
    (datafields, dataHeadersLocation) => {
      if (toAxisType(dataHeadersLocation) === axisType) {
        return datafields;
      }
      return null;
    }
  );

export const getRowUiAxis = createSelector(
  [
    getRowAxis,
    getAxisActivatedDatafields(AxisType.ROWS),
    state => state.axis.columns
  ],
  (rowAxis, activatedDatafields, crossFieldsCode) =>
    new AxisUi(
      rowAxis,
      {
        activatedDatafields,
        activatedDatafieldsCount: activatedDatafields
          ? activatedDatafields.length
          : 0
      },
      crossFieldsCode
    )
);

export const getColumnUiAxis = createSelector(
  [
    getColumnAxis,
    getAxisActivatedDatafields(AxisType.COLUMNS),
    state => state.axis.rows
  ],
  (columnAxis, activatedDatafields, crossFieldsCode) =>
    new AxisUi(
      columnAxis,
      {
        activatedDatafields,
        activatedDatafieldsCount: activatedDatafields
          ? activatedDatafields.length
          : 0
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
    getActivatedDatafieldsCount,
    state => state.config.dataHeadersLocation
  ],
  (rowAxis, columnAxis, rowsUi, columnsUi, activatedDatafieldsCount, dataHeadersLocation) => {
    const rowHorizontalCount = (rowAxis.fields.length || 1) +
      (dataHeadersLocation === 'rows' && activatedDatafieldsCount >= 1 ? 1 : 0);
    const rowVerticalCount = rowsUi.headers.length;
    const columnHorizontalCount = columnsUi.headers.length;
    const columnVerticalCount = (columnAxis.fields.length || 1) +
      (dataHeadersLocation === 'columns' && activatedDatafieldsCount >= 1
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
