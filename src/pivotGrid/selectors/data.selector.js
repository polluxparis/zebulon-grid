import { createSelector } from "reselect";
import { intersec } from "../utils/headers";
import { isUndefined, isDate } from "../utils/generic";
import { TOTAL_ID, MEASURE_ID } from "../constants";
import { availableDimensionsSelector } from "./dimensions.selector";

export const filteredIndexes = (dimensions, filters, loading) => {
  if (loading) {
    return undefined;
  }
  // const y = Date.now();
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
  data.forEach((row, index) => {
    if (
      filters.every(
        filter =>
          filter.values[dimensions[filter.dimensionId].keyAccessor({ row })] !==
          undefined
      )
    ) {
      // console.log("new index", index);
      filteredIndexes.set(index + offset, true);
    }
  });
};
export const notVisibleFiltersSelector = createSelector(
  [availableDimensionsSelector, state => state.filters],
  (dimensions, filters) =>
    dimensions.map(dimension => filters[dimension.id]).filter(filter => filter)
);
export const filteredIndexesSelector = createSelector(
  [
    state => state.dimensions,
    notVisibleFiltersSelector,
    state => state.status.loading
  ],
  filteredIndexes
);
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
      const key = dimension.keyAccessor({ row });
      if (isUndefined(values[key])) {
        const label = dimension.format({
          value: dimension.labelAccessor({ row })
        });
        const sortKey = dimension.sort.keyAccessor({ row });
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

export const indexedRowsSelector = createSelector(
  [state => state.data.data],
  data =>
    data.map((row, index) => {
      row.index_ = index;
      return row;
    })
);
export const computeData = (data, meta) => {
  const metaFormula = meta
    .filter(column => column.tp === "Computed")
    .forEach(column => (column.f = eval("(row)=>" + column.formula)));
  data.forEcach(row =>
    metaFormula.forEach(column => (row[column.id] = column.f(row)))
  );
};

export const metaSelector = createSelector(
  [state => state.data.data, state => state.data.meta],
  (data, meta, configurationFunctions) => {
    let position = 0;
    if (!meta && data.length) {
      const row = data[0];
      return Object.keys(row).map((key, index) => {
        let dataType = typeof row[key],
          filterType = "";
        if (dataType === "object" && isDate(row[key])) {
          dataType = "date";
          // format = dateToString;
        }
        let alignement = "unset";
        if (dataType === "string") {
          alignement = "left";
        } else if (dataType === "number") {
          alignement = "right";
          filterType = "between";
        } else if (dataType === "date") {
          alignement = "center";
          filterType = "between";
        }
        const width = 100;
        const meta = {
          id: key,
          caption: key,
          tp: "Initial",
          width,
          dataType,
          alignement,
          position,
          index_: index,
          formatFunction: ({ value }) => value,
          filterType,
          v: null,
          vTo: null
        };
        position += width;
        return meta;
      });
    } else {
      return meta;
    }
  }
);
