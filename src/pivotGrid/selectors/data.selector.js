import { createSelector } from 'reselect';
import { pass } from '../Filtering';

export const getFilters = state => state.filters || {};
const getData = state => state.data;

export const getFilteredData = createSelector(
  [getData, getFilters],
  (data, filtersObject) => {
    const filters = [
      ...Object.keys(filtersObject).map(id => filtersObject[id])
    ];
    if (filters.length === 0) {
      return data;
    }
    return data.filter(row =>
      filters.every(filter => pass(filter, row[filter.fieldId])));
  }
);
