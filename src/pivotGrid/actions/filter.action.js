import { ADD_FILTER, DELETE_FILTER } from "../constants";

export const addFilter = (dimensionId, operator, term, exclude, values) => ({
  type: ADD_FILTER,
  filter: { dimensionId, operator, term, exclude, values },
  dimension: dimensionId
});

export const deleteFilter = dimensionId => ({
  type: DELETE_FILTER,
  dimension: dimensionId
});
