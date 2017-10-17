import { createSelector } from "reselect";
import pass from "../utils/filtering";
import { isUndefined } from "../utils/generic";
import { TOTAL_ID, MEASURE_ID } from "../constants";

//         pass(filter.values, filter.dimension.keyAccessor(row))
//       )
//       filters.every(filter =>
//     "filteredDataSelector",
//     );
//     const data = getFilteredData(pushedData, filters, dimensions);
//     d = data.filter(row =>
//     data.length,
//     Date.now() - x
//     dimension: dimensions[id],
//     filtersObject.length,
//     return data;
//     values: Object.values(filtersObject[id].values)
//   (pushedData, filters, dimensions) => {
//   );
//   // console.log("filteredDataSelector0", filtersObject.length, data.length, x);
//   [getData, getFilters, state => state.dimensions],
//   [state => state.data.pushedData, getFilters, state => state.dimensions],
//   console.log(
//   const filters = Object.keys(filtersObject).map(id => ({
//   const x = Date.now();
//   getFilteredData
//   if (filters.length !== 0) {
//   let d = data;
//   return d;
//   }
//   }
//   }));
// );
// );
// const getData = state => state.data.data;
// const getFilters = state => state.filters || {};
// export const filteredDataSelector = createSelector(
// export const filteredPushedDataSelector = createSelector(
// export const getFilteredData = (data, filtersObject, dimensions) => {
// };

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
