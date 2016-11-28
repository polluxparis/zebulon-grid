import { ExpressionFilter } from '../Filtering';

export const ADD_FILTER = 'ADD_FILTER';
export const addFilter =
  (fieldId, operator, term, staticValue, excludeStatic) => ({
    type: ADD_FILTER,
    filter: new ExpressionFilter(fieldId, operator, term, staticValue, excludeStatic),
    field: fieldId,
  });

export const DELETE_FILTER = 'DELETE_FILTER';
export const deleteFilter = fieldId => ({
  type: DELETE_FILTER,
  field: fieldId,
});
