import { createSelector } from 'reselect';
import { pass } from '../Filtering';
import { isDate, isNumber } from '../utils/generic';

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

export const getFieldValues = createSelector([getData], data =>
  (field, filterFunc) => {
    const values = [];
    const labels = [];
    let res = [];
    const labelsMap = {};
    const valuesMap = {};
    let containsBlank = false;
    // We use data here instead of filteredData
    // Otherwise you lose the filtered values the next time you open a Filter Panel
    for (let i = 0; i < data.length; i += 1) {
      const row = data[i];
      const val = row[field.id];
      const label = row[field.name];
      labelsMap[val] = label;
      valuesMap[label] = val;
      if (filterFunc !== undefined) {
        if (
          filterFunc === true ||
          (typeof filterFunc === 'function' && filterFunc(val))
        ) {
          values.push(val);
          labels.push(label);
        }
      } else if (val != null) {
        values.push(val);
        labels.push(label);
      } else {
        containsBlank = true;
      }
    }
    if (labels.length > 1) {
      if (isNumber(labels[0]) || isDate(labels[0])) {
        labels.sort((a, b) => {
          if (a) {
            if (b) {
              return a - b;
            }
            return 1;
          }
          if (b) {
            return -1;
          }
          return 0;
        });
      } else {
        labels.sort();
      }

      for (let vi = 0; vi < labels.length; vi += 1) {
        if (vi === 0 || labels[vi] !== res[res.length - 1].label) {
          res.push({ value: valuesMap[labels[vi]], label: labels[vi] });
        }
      }
    } else {
      res = values.map(value => ({ value, label: labelsMap[value] }));
    }
    if (containsBlank) {
      res.unshift({ value: null, label: '' });
    }
    return res;
  });
