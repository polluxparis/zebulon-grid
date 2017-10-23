import { createSelector } from "reselect";
// import pass from "../utils/filtering";
import { isUndefined } from "../utils/generic";
import { TOTAL_ID, MEASURE_ID } from "../constants";

export const getDimensionValuesSelector = createSelector(
  [state => state.data.data, state => state.dimensions],
  (data, dimensions) => id => {
    if (id === TOTAL_ID || id === MEASURE_ID) {
      return [];
    }
    const dimension = dimensions[id];
    let values = {};
    // We use data here instead of filteredData
    // Otherwise you lose the filtered values the next time you open a Filter Panel
    for (let i = 0; i < data.length; i += 1) {
      const row = data[i];
      const key = dimension.keyAccessor(row);
      if (isUndefined(values[key])) {
        const label = String(dimension.labelAccessor(row));
        const sortKey = dimension.sort.keyAccessor(row);
        values[key] = {
          id: key,
          label: label,
          sortKey: sortKey
        };
      }
    }
    values = Object.keys(values).map(key => values[key]);
    let sortFunction;
    if (dimension.sort.custom) {
      sortFunction = (a, b) => dimension.sort.custom(a.sortKey, b.sortKey);
    } else {
      sortFunction = (a, b) =>
        (a.sortKey > b.sortKey) - (b.sortKey > a.sortKey);
    }
    values.sort(sortFunction);
    return values;
  }
);

export const dimensionFiltersSelector = createSelector(
  [state => state.filters],
  filters => dimensionId => filters[dimensionId]
);
