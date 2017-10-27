// import { createSelector } from 'reselect';
// import pass from '../utils/filtering';
// import { isUndefined } from '../utils/generic';

// const getFilters = state => state.filters || {};
// const getData = state => state.data;

// export const filteredDataSelector = createSelector(
//   [getData, getFilters, state => state.dimensions],
//   (data, filtersObject, dimensions) => {
//     const filters = [
//       ...Object.keys(filtersObject).map(id => filtersObject[id])
//     ];
//     if (filters.length === 0) {
//       return data;
//     }
//     return data.filter(row =>
//       filters.every(filter =>
//         pass(filter, dimensions[filter.dimensionId].keyAccessor(row))
//       )
//     );
//   }
// );
import { createSelector } from "reselect";
import { intersec } from "../utils/headers";
import { isUndefined } from "../utils/generic";
import { TOTAL_ID, MEASURE_ID } from "../constants";

export const filteredIndexes = (dimensions, filters, loading) => {
  if (loading) {
    return undefined;
  }
  const y = Date.now();
  if (filters.length) {
    const filteredIndexes = filters.map(filter => {
      const dimension = dimensions[filter.dimensionId];
      let dimensionIndexes = [];
      if (!dimension.axis) {
        dimensionIndexes = Object.values(
          dimension.values
        ).reduce((indexes, value) => {
          if (filter.values[value.id] !== undefined) {
            indexes = indexes.concat(value.rowIndexes);
          }
          return indexes;
        }, []);
      }
      return dimensionIndexes.sort((a, b) => a - b);
    });
    const x = new Map(intersec(filteredIndexes).map(index => [index, true]));
    console.log("filteredIndexes", Date.now() - y), x.length;
    return x;
  } else {
    return undefined;
  }
};
export const dataFilteredIndexes = (
  data,
  filters,
  dimensions,
  filteredIndexes,
  offset
) => {
  data.map((row, index) => {
    if (
      filters.every(
        filter =>
          filter.values[dimensions[filter.dimensionId].keyAccessor(row)] !==
          undefined
      )
    ) {
      console.log("new index", index);
      filteredIndexes.set(index + offset, true);
    }
  });
};
export const notVisibleFiltersSelector = createSelector(
  [state => state.dimensions, state => state.filters],
  (dimensions, filters) =>
    Object.values(filters).filter(
      filter => !dimensions[filter.dimensionId].axis
    )
);
// only filters on not visible dimensions
// export const dataSelector = createSelector(
//   [state => state.data.data, state => state.status.loading],
//   data => data
// );

export const filteredIndexesSelector = createSelector(
  [
    state => state.dimensions,
    notVisibleFiltersSelector,
    state => state.status.loading
  ],
  filteredIndexes
);
// export const filteredDataSelector = createSelector(
//   [dataSelector, filteredIndexesSelector],
//   (data, filteredIndexes) => {
//     if (filteredIndexes) {
//       // const y = Date.now();
//       // data.map(row => (row.isFiltered = true));
//       // filteredIndexes.map(index => (data[index].isFiltered = false));
//       // console.log("filteredDataSelector", Date.now() - y),
//       //   filteredIndexes.length;
//       return filteredIndexes;
//     }
//   }
// );
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
