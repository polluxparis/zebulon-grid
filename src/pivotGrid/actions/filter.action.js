import { ADD_FILTER, DELETE_FILTER } from '../constants';

export const addFilter = (
  dimensionId,
  operator,
  term,
  staticValue,
  excludeStatic
) => {
  const a = 1;
  return {
    type: ADD_FILTER,
    filter: { dimensionId, operator, term, staticValue, excludeStatic },
    dimension: dimensionId
  };
};

export const deleteFilter = dimensionId => ({
  type: DELETE_FILTER,
  dimension: dimensionId
});
