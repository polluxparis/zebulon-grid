import { createSelector } from "reselect";
import { isNullOrUndefined, isUndefined, range } from "../utils/generic";
import { intersectDataIndexes } from "../utils/headers";
// import { filteredDataSelector } from "./data.selector";
import {
  activatedMeasuresSelector,
  rowVisibleDimensionsSelector,
  columnVisibleDimensionsSelector
} from "./dimensions.selector";
import { rowLeavesSelector, columnLeavesSelector } from "./axis.selector";
import { MEASURE_ID, HeaderType } from "../constants";

const cellValue = (
  data,
  valueAccessor,
  rowDataIndexes,
  columnDataIndexes,
  aggregation = () => null
) => {
  const intersection = intersectDataIndexes(rowDataIndexes, columnDataIndexes);
  // Root headers have undefined data indexes
  const intersectionArray = isUndefined(intersection)
    ? range(0, data.length - 1)
    : intersection;
  // Remove rows for which the accessor gives a null or undefined value
  // This allows better behaviour for cells which have a null value
  // for example getting an empty cell instead of zero
  const intersectionWithNonNullOrUndefinedValue = intersectionArray.filter(
    i => !isNullOrUndefined(valueAccessor(data[i]))
  );
  // If we assume that all our measures are numerical we can be more strict
  // and keep only rows where the accessor gives a finite number
  // This removes Javascript operators weird behaviour with non finite number values.
  // Design decision to be made later.
  //  const intersectionWithNumericalValue = intersectionArray.filter(
  //   i => Number.isFinite(accessor(data[i]))
  // );
  return aggregation(
    valueAccessor,
    intersectionWithNonNullOrUndefinedValue,
    data
  );
};

export const getCellValueSelector = createSelector(
  [state => state.data.data],
  data => (
    valueAccessor,
    rowDataIndexes,
    columnDataIndexes,
    aggregation = () => null
  ) => {
    return cellValue(
      data,
      valueAccessor,
      rowDataIndexes,
      columnDataIndexes,
      aggregation
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
          id: l.isParentCollapsed ? null : l.id,
          caption: l.isParentCollapsed ? null : l.caption
        }
      });

      l = l.parent;
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
    const rowLeaf = rowLeaves.leaves[cell.rowIndex];
    const columnLeaf = columnLeaves.leaves[cell.columnIndex];
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
    const usedData = intersectDataIndexes(
      rowLeaf.dataIndexes,
      columnLeaf.dataIndexes
    ).map(x => data[x]);
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
      isNullOrUndefined(rg.selectedCellStart) ||
      isNullOrUndefined(rg.selectedCellEnd)
    ) {
      return {};
    }
    const columnDims = columnDimensions.filter(column => column.isVisible);
    const rowDims = rowDimensions.filter(row => row.isVisible);
    const range = {
      selectedCellStart: {
        rowIndex: Math.min(
          rg.selectedCellStart.rowIndex,
          rg.selectedCellEnd.rowIndex
        ),
        columnIndex: Math.min(
          rg.selectedCellStart.columnIndex,
          rg.selectedCellEnd.columnIndex
        )
      },
      selectedCellEnd: {
        rowIndex: Math.max(
          rg.selectedCellStart.rowIndex,
          rg.selectedCellEnd.rowIndex
        ),
        columnIndex: Math.max(
          rg.selectedCellStart.columnIndex,
          rg.selectedCellEnd.columnIndex
        )
      }
    };
    const rows = {};
    for (
      let index = range.selectedCellStart.rowIndex;
      index <= range.selectedCellEnd.rowIndex;
      index += 1
    ) {
      const rowLeaf = rowLeaves[index];
      rows[index] = cellDimensionInfos(
        data,
        rowDims,
        "rows",
        rowLeaf,
        measures
      );
    }
    const columns = {};
    for (
      let index = range.selectedCellStart.columnIndex;
      index <= range.selectedCellEnd.columnIndex;
      index += 1
    ) {
      const columnLeaf = columnLeaves[index];
      columns[index] = cellDimensionInfos(
        data,
        columnDims,
        "columns",
        columnLeaf,
        measures
      );
    }
    const values = {};
    for (
      let columnIndex = range.selectedCellStart.columnIndex;
      columnIndex <= range.selectedCellEnd.columnIndex;
      columnIndex += 1
    ) {
      const columnLeaf = columnLeaves[columnIndex];
      values[columnIndex] = {};
      for (
        let rowIndex = range.selectedCellEnd.rowIndex;
        rowIndex <= range.selectedCellEnd.rowIndex;
        rowIndex += 1
      ) {
        const rowLeaf = rowLeaves[rowIndex];
        const measure =
          rowLeaf.type === HeaderType.MEASURE
            ? measures[rowLeaf.id]
            : measures[columnLeaf.id];
        const value = cellValue(
          data,
          measure.valueAccessor,
          rowLeaf.dataIndexes,
          columnLeaf.dataIndexes,
          measure.aggregation
        );
        values[columnIndex][rowIndex] = value;

        // const usedData = intersectDataIndexes(
        //   rowLeaf.dataIndexes,
        //   columnLeaf.dataIndexes
        // ).map(x => data[x]);
      }
    }
    return { values, columns, rows, range, measureHeadersAxis };
  }
);
