import { createSelector } from 'reselect';

export const getRowFields = createSelector(
  [state => state.axis.rows, state => state.fields],
  (rowAxis, fields) => rowAxis.map(id => fields[id])
);

export const getColumnFields = createSelector(
  [state => state.axis.columns, state => state.fields],
  (columnAxis, fields) => columnAxis.map(id => fields[id])
);

export const getAvailableFields = createSelector(
  [state => state.axis.fields, state => state.fields],
  (fieldAxis, fields) => fieldAxis.map(id => fields[id])
);

const getDataFields = state => state.datafields;

export const getActivatedDataFields = createSelector(
  [getDataFields],
  datafields =>
    Object.keys(datafields)
      .map(id => datafields[id])
      .filter(field => field.activated)
);
