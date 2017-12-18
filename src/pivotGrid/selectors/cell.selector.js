import { createSelector } from "reselect";
import { isNullOrUndefined, isUndefined, range } from "../utils/generic";
import { inter } from "../utils/headers";
// import { dataSelector } from "./data.selector";
import {
  activatedMeasuresSelector,
  rowVisibleDimensionsSelector,
  columnVisibleDimensionsSelector
} from "./dimensions.selector";
import { rowLeavesSelector, columnLeavesSelector } from "./axis.selector";
import { MEASURE_ID, ROOT_ID, HeaderType } from "../constants";
export const getFilteredIndex = leaf => {
  if (leaf.isParentCollapsed) {
    return getFilteredIndex(leaf.parent);
  } else {
    return leaf.filteredIndexes;
  }
};
const cellValue = (
  data,
  valueAccessor,
  rowDataIndexes,
  columnDataIndexes,
  aggregation = () => null,
  measureId
) => {
  const intersection = inter(rowDataIndexes, columnDataIndexes);
  // Root headers have undefined data indexes
  const intersectionArray = isUndefined(intersection)
    ? range(0, data.length - 1)
    : intersection;
  // Remove rows for which the accessor gives a null or undefined value
  // This allows better behaviour for cells which have a null value
  // for example getting an empty cell instead of zero
  let edited = false,
    row = {},
    comments = [],
    nullLastNewValue = false;

  const values = intersectionArray
    // .map(index => (data[index].isFiltered ? null : valueAccessor(data[index])))
    .map(index => {
      row = data[index];
      let comment = row._comment;
      if (measureId && row._editedMeasure === measureId) {
        if (row._newValue !== row._oldValue) {
          edited = true;
          nullLastNewValue = row._newValue === null;
          // row._edited = true;
          comment =
            "Value from " +
            String(row._oldValue) +
            " to " +
            String(row._newValue) +
            (comment ? "\n" + comment : "");
        } else {
          nullLastNewValue = false;
        }
        if (comment) {
          comments.push(comment);
        }
      }
      return valueAccessor(data[index]);
    })
    .filter(value => !isNullOrUndefined(value));
  // If we assume that all our measures are numerical we can be more strict
  // and keep only rows where the accessor gives a finite number
  // This removes Javascript operators weird behaviour with non finite number values.
  // Design decision to be made later.
  //  const intersectionWithNumericalValue = intersectionArray.filter(
  //   i => Number.isFinite(accessor(data[i]))
  // );
  let value = aggregation(values, data);
  if (value === 0 && nullLastNewValue) {
    value = null;
  }
  return { edited, comments, value };
};

export const getCellValueSelector = createSelector(
  [state => state.data.data],
  data => (
    valueAccessor,
    rowDataIndexes,
    columnDataIndexes,
    aggregation = () => null,
    measureId
  ) => {
    return cellValue(
      data,
      valueAccessor,
      rowDataIndexes,
      columnDataIndexes,
      aggregation,
      measureId
    );
  }
);

const cellDimensionInfos = (data, axisDimensions, axis, leaf, measures) => {
  let l = leaf;
  const depth = axisDimensions.length;
  let dimension;
  const dimensions = [];
  // const row = isNullOrUndefined(leaf.dataIndexes)
  // ? null
  // : data[leaf.dataIndexes[0]];
  for (let index = depth - 1; index >= 0; index -= 1) {
    dimension = axisDimensions[index];

    // when a leaf is collapsed its parent  has a depth < leaf.depth -1
    // we  have to push an empty cell in this case
    // if (index > l.depth) {
    //   dimensions.push({
    //     axis,
    //     dimension: {
    //       id: dimension.id,
    //       caption: dimension.caption,
    //       isCollapsed: leaf.isParentCollapsed
    //     },
    //     cell: { id: null, caption: null }
    //   });
    // } else
    if (dimension.id === MEASURE_ID) {
      dimensions.push({
        axis,
        dimension: {
          id: dimension.id,
          caption: "measures",
          isCollapsed: false
        },
        cell: { id: l.id, caption: measures[l.id].caption }
      });
      l = l.parent;
    } else {
      dimensions.push({
        axis,
        dimension: {
          id: dimension.id,
          caption: dimension.caption,
          isCollapsed: l.isParentCollapsed
        },
        cell: {
          id: l.isParentCollapsed || l.depth !== index ? null : l.id,
          caption: l.isParentCollapsed || l.depth !== index ? null : l.caption
        }
      });

      if (l.depth === index) {
        l = l.parent;
      }
    }
  }
  return dimensions.reverse();
};
export const getCellDimensionInfosSelector = createSelector(
  [state => state.data.data],
  data => (axisDimensions, axis, leaf, measures, dimensions) => {
    return cellDimensionInfos(
      data,
      axisDimensions,
      axis,
      leaf,
      measures,
      dimensions
    );
  }
);

export const getCellInfosSelector = createSelector(
  [
    state => state.data.data,
    rowLeavesSelector,
    columnLeavesSelector,
    activatedMeasuresSelector,
    rowVisibleDimensionsSelector,
    columnVisibleDimensionsSelector
  ],
  (
    data,
    rowLeaves,
    columnLeaves,
    measures,
    rowDimensions,
    columnDimensions
  ) => cell => {
    const columns = columnDimensions.filter(column => column.isVisible);
    const rows = rowDimensions.filter(row => row.isVisible);
    const rowLeaf = rowLeaves.leaves[cell.rows];
    const columnLeaf = columnLeaves.leaves[cell.columns];
    let measure;
    if (rowLeaf.type === HeaderType.MEASURE) {
      measure = measures[rowLeaf.id];
    } else {
      measure = measures[columnLeaf.id];
    }
    const value = cellValue(
      data,
      measure.valueAccessor,
      rowLeaf.dataIndexes,
      columnLeaf.dataIndexes,
      measure.aggregation
    );
    let dimensions = [];
    dimensions = cellDimensionInfos(data, rows, "rows", rowLeaf, measures);
    dimensions = dimensions.concat(
      cellDimensionInfos(data, columns, "columns", columnLeaf, measures)
    );
    const usedData = inter(rowLeaf.dataIndexes, columnLeaf.dataIndexes).map(
      x => data[x]
    );
    return { value, dimensions, data: usedData };
  }
);
export const getRangeInfosSelector = createSelector(
  [
    state => state.data.data,
    rowLeavesSelector,
    columnLeavesSelector,
    activatedMeasuresSelector,
    rowVisibleDimensionsSelector,
    columnVisibleDimensionsSelector,
    state => state.configuration.measureHeadersAxis
  ],
  (
    data,
    rowLeaves,
    columnLeaves,
    measures,
    rowDimensions,
    columnDimensions,
    measureHeadersAxis
  ) => rg => {
    if (
      isNullOrUndefined(rg) ||
      isNullOrUndefined(rg.start) ||
      isNullOrUndefined(rg.end)
    ) {
      return {};
    }
    const columnDims = columnDimensions.filter(column => column.isVisible);
    const rowDims = rowDimensions.filter(row => row.isVisible);
    const range = {
      start: {
        rows: Math.min(rg.start.rows, rg.end.rows),
        columns: Math.min(rg.start.columns, rg.end.columns)
      },
      end: {
        rows: Math.max(rg.start.rows, rg.end.rows),
        columns: Math.max(rg.start.columns, rg.end.columns)
      }
    };
    const rows = [];
    // letix = 0;
    for (let index = range.start.rows; index <= range.end.rows; index += 1) {
      const rowLeaf = rowLeaves.leaves[index];
      if (rowLeaf.isVisible) {
        rowLeaf.dataIndexes = getFilteredIndex(rowLeaf);
        const row = cellDimensionInfos(
          data,
          rowDims,
          "rows",
          rowLeaf,
          measures
        );
        row.leaf = rowLeaf;
        rows.push(row);
      }
    }
    const columns = [];
    for (
      let index = range.start.columns;
      index <= range.end.columns;
      index += 1
    ) {
      const columnLeaf = columnLeaves.leaves[index];
      if (columnLeaf.isVisible) {
        columnLeaf.dataIndexes = getFilteredIndex(columnLeaf);
        const column = cellDimensionInfos(
          data,
          columnDims,
          "columns",
          columnLeaf,
          measures
        );
        column.leaf = columnLeaf;
        columns.push(column);
      }
    }
    const values = [];
    rows.map((row, index) => {
      values.push([]);
      columns.map(column => {
        const measure =
          row.leaf.type === HeaderType.MEASURE
            ? measures[row.leaf.id]
            : measures[column.leaf.id];
        const value = cellValue(
          data,
          measure.valueAccessor,
          row.leaf.dataIndexes,
          column.leaf.dataIndexes,
          measure.aggregation
        );
        values[index].push(value);
      });
    });
    return { values, columns, rows, range, measureHeadersAxis };
  }
);
const buildData = (dimensions, measures, leaf, value, data) => {
  if (leaf.dimensionId === MEASURE_ID) {
    data[measures[leaf.id].datasetProperties.value] = value;
  } else {
    const properties = dimensions[leaf.dimensionId].datasetProperties;
    data[properties.id] = leaf.id;
    data[properties.label] = leaf.caption;
    data[properties.sort] = leaf.sortKey;
  }
  const parent = leaf.parent;
  if (parent && parent.id !== ROOT_ID) {
    buildData(dimensions, measures, parent, value, data);
  }
};
export const buildDataSelector = createSelector(
  [
    state => state.dimensions,
    state => state.measures,
    state => state.configuration.measureHeadersAxis
  ],
  (dimensions, measures, measureHeadersAxis) => (
    rowLeaf,
    columnLeaf,
    oldValue,
    newValue,
    comment
  ) => {
    const data = {};
    const value = newValue - oldValue;

    buildData(dimensions, measures, rowLeaf, value, data);
    buildData(dimensions, measures, columnLeaf, value, data);
    data._editedMeasure =
      measureHeadersAxis === "rows" ? rowLeaf.id : columnLeaf.id;
    if (newValue !== oldValue) {
      data._oldValue = oldValue;
      data._newValue = newValue;
    }
    if (!(isNullOrUndefined(comment) || comment === "")) {
      data._comment = comment;
    }
    return data;
  }
);
