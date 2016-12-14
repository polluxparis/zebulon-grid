import { ADD_FILTER, DELETE_FILTER } from '../constants';

export const addFilter =
  (fieldId, operator, term, staticValue, excludeStatic) => ({
    type: ADD_FILTER,
    filter: { fieldId, operator, term, staticValue, excludeStatic },
    field: fieldId,
  });

export const deleteFilter = fieldId => ({
  type: DELETE_FILTER,
  field: fieldId,
});
