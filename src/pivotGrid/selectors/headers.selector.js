///////////////////////////////////////////////////////////////////
//  compute the header trees (rows and columns) with positions ans sizes...
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";
import { isNull } from "../utils/generic";

import {
  buildAxisTrees,
  getAxisTreesSelector,
  getAxisLeaves,
  rowLeavesSelector,
  columnLeavesSelector,
  getAxisActivatedMeasuresSelector
} from "./axis.selector";
import {
  rowVisibleDimensionsSelector,
  columnVisibleDimensionsSelector
} from "./dimensions.selector";

import {
  crossRowPositionsSelector,
  crossColumnPositionsSelector,
  getCellHeightByKeySelector,
  getCellWidthByKeySelector
} from "./cellSizes.selector";
import {
  rowHeadersWidthSelector,
  columnHeadersHeightSelector
} from "./sizes.selector";
import {
  ROOT_ID,
  TOTAL_ID,
  MEASURE_ID,
  ScrollbarSize,
  AxisType
} from "../constants";

///////////////////////////////////////////////////////////////////
// headers
//////////////////////////////////////////////////////////////////
const parentsSizes = (
  header,
  main,
  crossPositions,
  collapsedSizes,
  affix,
  cellsKey,
  rowCells,
  dimensions,
  measuresCount,
  direction
) => {
  if (header.id === ROOT_ID) {
    return null;
  }
  // et main = mainSizes;
  const first = cellsKey[header.key] === undefined;
  if (first) {
    header.sizes = {
      main: { ...main },
      cross: { ...crossPositions[dimensions[header.depth].id] }
    };
    header.options = {
      isNotCollapsible:
        header.isAttribute ||
        header.depth >= getLastDimension(dimensions) ||
        header.span === measuresCount,
      isAffixManaged: false,
      isDropTarget: false
    };
    cellsKey[header.key] = true;
  } else {
    header.sizes.main.size += main.size;
    if (direction === -1) {
      header.sizes.main.position -= main.size;
    }
  }
  header.sizes.affix = header.nVisibles === 1 ? 0 : affix;
  if (header.parent.id !== ROOT_ID) {
    parentsSizes(
      header.parent,
      main,
      crossPositions,
      collapsedSizes,
      affix,
      cellsKey,
      rowCells,
      dimensions,
      measuresCount,
      direction
    );
  }
  if (first) {
    if (
      dimensions[header.depth].isVisible &&
      (header.parent.id === ROOT_ID || !header.isParentCollapsed)
    ) {
      rowCells.push(header);
    }
    if (header.isCollapsed) {
      header.sizes.cross.collapsed = collapsedSizes[header.depth];
      header.sizes.cross.size += collapsedSizes[header.depth];
    }
    if (header.isParentCollapsed) {
      header.dataIndexes = header.parent.dataIndexes;
    } else {
      header.dataIndexes = header.dataRowIndexes;
    }
  }
  return header.rootIndex;
};

const getLastDimension = dimensions =>
  dimensions.length -
  1 -
  ((dimensions[dimensions.length - 1] || {}).id === MEASURE_ID);
const getCollapsedSizes = (dimensions, crossPositions) => {
  // pre compute collapses size
  const collapsedSizes = [];
  let collapsedSize = 0,
    isPrevAttribute = false,
    size;
  for (let index = getLastDimension(dimensions); index >= 0; index--) {
    if (isPrevAttribute) {
      collapsedSizes.push(0);
    } else {
      collapsedSizes.push(collapsedSize);
    }
    size = crossPositions[dimensions[index].id].size; // headerSizes({ index: [index] });
    collapsedSize += size;
    isPrevAttribute =
      dimensions[index].isAttribute && dimensions[index].isVisible;
  }
  collapsedSizes.reverse();
  return collapsedSizes;
};
const buildPositionedHeaders = (
  headerSizes,
  crossPositions,
  containerSize,
  crossSize,
  dimensions,
  measures,
  nVisibles,
  leaves,
  scroll
) => {
  if (!leaves.length) {
    return undefined;
  }
  const x = Date.now();
  const maxSize = containerSize - crossSize;
  let size = 0,
    depth = 0;
  const measuresCount = isNull(measures)
    ? 1
    : Object.keys(measures).length || 1;
  // no dimension is on the axis ==> Total header
  let cellsKey = {};
  const collapsedSizes = getCollapsedSizes(dimensions, crossPositions);
  const cells = [];
  // Loop on leaves
  if (scroll.index >= leaves.length) {
    scroll.index = leaves.length - 1;
    scroll.direction = -1;
  }
  let index = scroll.index;
  let ix = 0;
  let lastSize;
  while (size <= maxSize && index < leaves.length && index >= 0) {
    // leaves.map((header, index) => {
    const header = leaves[index];
    const dimension = dimensions[header.depth];
    header.index = index;
    if (header.isVisible) {
      // if (header.relativeIndex < measuresCount) {
      // if (true) {
      const rowCells = [];
      const mainSize = headerSizes(header.key);
      header.sizes = {
        main: {
          size: mainSize,
          position: scroll.direction === 1 ? size : maxSize - size - mainSize
        },
        cross: crossPositions[dimension.id]
      };
      parentsSizes(
        header.parent,
        header.sizes.main,
        crossPositions,
        collapsedSizes,
        scroll.direction === 1 ? false : size + mainSize > maxSize,
        cellsKey,
        rowCells,
        // areCollapsed,
        dimensions,
        measuresCount,
        scroll.direction
      );
      header.options = {
        isNotCollapsible: true,
        isAffixManaged: false,
        isDropTarget:
          (dimension.id === MEASURE_ID || dimension.id === TOTAL_ID) &&
          (header.depth === 0 || measuresCount > 1)
      };
      if (header.isParentCollapsed) {
        header.dataIndexes = header.parent.dataIndexes;
      } else {
        header.dataIndexes = header.dataRowIndexes;
      }
      lastSize = header.sizes.main.size;
      if (dimension.isVisible) {
        rowCells.push(header);
      }
      if (rowCells.length) {
        rowCells.reverse();
        cells.push(rowCells);
        size += header.sizes.main.size;
        ix++;
      }
    }
    index += scroll.direction;
  }
  // when height(or width) increase and the grid is scrolled, it may not rest enough cells to feed the grid
  if (
    size < maxSize &&
    (Math.max(index, scroll.index) < leaves.length - 1 ||
      Math.min(index, scroll.index) > 0)
  ) {
    scroll.index = leaves.length - 1;
    scroll.direction = -1;
    return buildPositionedHeaders(
      headerSizes,
      crossPositions,
      containerSize,
      crossSize,
      dimensions,
      measures,
      nVisibles,
      leaves,
      scroll
    );
  }
  console.log("buildPositionedHeaders", Date.now() - x, scroll, cells);
  return {
    size: Math.min(maxSize, size),
    containerSize,
    crossSize,
    lastSize,
    affix: scroll.direction === 1 ? 0 : Math.max(size - maxSize, 0),
    depth: depth + 1,
    cells,
    index: scroll.index,
    direction: scroll.direction,
    startIndex: scroll.direction === 1 ? scroll.index : index,
    stopIndex: scroll.direction === -1 ? scroll.index : index,
    length: leaves.length,
    leaves,
    // headers,
    nVisibles,
    hasScrollbar: ix < nVisibles || size > maxSize,
    // displayedRatio: (ix - (size > maxSize)) / nVisibles,
    displayedRatio:
      cells.length - (size > maxSize + 3) === nVisibles
        ? maxSize / Math.min(maxSize, size)
        : cells.length / nVisibles,
    positionRatio:
      (scroll.direction === 1 ? scroll.index : index) / leaves.length,
    scroll
  };
};

export const getRowHeadersSelector = createSelector(
  [
    getCellHeightByKeySelector,
    crossColumnPositionsSelector,
    state => state.config.height,
    columnHeadersHeightSelector,
    rowVisibleDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    state => state.selectedRange.scrollToRow.refreshLeaves
  ],
  (
    headerSizes,
    crossPositions,
    containerSize,
    crossSize,
    dimensions,
    measures,
    x
  ) => (leaves, scroll) => {
    return buildPositionedHeaders(
      headerSizes,
      crossPositions,
      containerSize,
      crossSize,
      dimensions,
      measures,
      leaves.node.nVisibles,
      leaves.leaves,
      scroll
    );
  }
);
export const getColumnHeadersSelector = createSelector(
  [
    getCellWidthByKeySelector,
    crossRowPositionsSelector,
    state => state.config.width,
    rowHeadersWidthSelector,
    columnVisibleDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    state => state.selectedRange.scrollToColumn.refreshLeaves
  ],
  (
    headerSizes,
    crossPositions,
    containerSize,
    crossSize,
    dimensions,
    measures,
    x
  ) => (leaves, scroll) => {
    return buildPositionedHeaders(
      headerSizes,
      crossPositions,
      containerSize,
      crossSize,
      dimensions,
      measures,
      leaves.node.nVisibles,
      leaves.leaves,
      scroll
    );
  }
);
const combineRowsAndColumns = (rows, columns) => {
  console.log(rows, columns);
  if (
    (rows.hasScrollbar && !columns.hasScrollbar) ||
    (!rows.hasScrollbar && columns.hasScrollbar)
  ) {
    if (rows.hasScrollbar && columns.size + ScrollbarSize > columns.maxSize) {
      columns.hasScrollbar = true;
    } else if (
      columns.hasScrollbar &&
      rows.size + ScrollbarSize > rows.maxSize
    ) {
      rows.hasScrollbar = true;
    }
  }
  columns.offset = rows.hasScrollbar
    ? columns.scroll.direction === 1 ? 0 : ScrollbarSize
    : 0;
  rows.offset = columns.hasScrollbar
    ? rows.scroll.direction === 1 ? 0 : ScrollbarSize
    : 0;
};

export const pushedDataSelector = createSelector(
  [
    state => state.data,
    getAxisTreesSelector,
    rowLeavesSelector,
    columnLeavesSelector,
    state => state.dimensions,
    state => state.axis,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    state => state.collapses
  ],
  (
    data,
    axisTrees,
    rowLeaves,
    columnLeaves,
    dimensions,
    axises,
    rowMeasures,
    columnMeasures,
    collapses
  ) => {
    // const filteredPushedData = getFilteredPushedData(data, filters, dimensions);
    if (!data.pushedData.length) {
      return { rows: rowLeaves, columns: columnLeaves };
    }
    const newAxisTrees = buildAxisTrees(
      axisTrees.rows,
      axisTrees.columns,
      data.pushedData,
      axisTrees.dimensions,
      data.data.length
    );
    let leaves, axisDimensions;
    if (newAxisTrees.newRows) {
      leaves = [];
      axisDimensions = axises.rows.map(axis => dimensions[axis]);
      getAxisLeaves(
        axisTrees.rows,
        axisDimensions,
        rowMeasures,
        rowMeasures === null ? 1 : rowMeasures.length,
        collapses.rows,
        0,
        leaves
      );
      rowLeaves.leaves = leaves;
    }
    if (newAxisTrees.newColumns) {
      leaves = [];
      axisDimensions = axises.columns.map(axis => dimensions[axis]);
      getAxisLeaves(
        axisTrees.columns,
        axisDimensions,
        columnMeasures,
        columnMeasures === null ? 1 : columnMeasures.length,
        collapses.columns,
        0,
        leaves
      );
      columnLeaves.leaves = leaves;
    }
    data.data.push(...data.pushedData);
    data.pushedData = [];
    return { rows: rowLeaves, columns: columnLeaves };
  }
);

export const rowAndColumnHeadersSelector = createSelector(
  [
    pushedDataSelector,
    getRowHeadersSelector,
    state => state.selectedRange.scrollToRow,
    getColumnHeadersSelector,
    state => state.selectedRange.scrollToColumn
  ],
  (
    rowAndColumnLeaves,
    getRowHeaders,
    scrollToRow,
    getColumnHeaders,
    scrollToColumn
  ) => {
    let rows, columns;
    if (
      !(
        rowAndColumnLeaves.rows.node === null ||
        rowAndColumnLeaves.columns.node === null
      )
    ) {
      rows = getRowHeaders(rowAndColumnLeaves.rows, scrollToRow);
      columns = getColumnHeaders(rowAndColumnLeaves.columns, scrollToColumn);
      combineRowsAndColumns(rows, columns);
      // corrections of diplay aberations
      let change = false;
      if (
        columns.direction === -1 &&
        columns.cells[columns.cells.length - 1][0].sizes.main.position -
          columns.offset >
          0
      ) {
        scrollToColumn.index = 0;
        scrollToColumn.direction = 1;
        columns = getColumnHeaders(rowAndColumnLeaves.columns, scrollToColumn);
        change = true;
      }
      if (
        rows.direction === -1 &&
        rows.cells[rows.cells.length - 1][0].sizes.main.position - rows.offset >
          0
      ) {
        scrollToRow.index = 0;
        scrollToRow.direction = 1;
        rows = getRowHeaders(rowAndColumnLeaves.rows, scrollToRow);
        change = true;
      }
      if (change) {
        combineRowsAndColumns(rows, columns);
      }
    }
    return { rows, columns };
  }
);

export const getRowsCollapsedSelector = createSelector(
  [rowLeavesSelector, state => state.collapses.columns],
  (leaves, areCollapsed) => (maxSize, scroll) => {}
);

export const previewSizesSelector = createSelector(
  [
    state => state.config.height,
    state => state.config.width,
    rowAndColumnHeadersSelector
  ],
  (height, width, rowsAndColumns) => {
    const { rows, columns } = rowsAndColumns;
    console.log(
      height,
      width,
      columns.crossSize,
      columns.size,
      rows.crossSize,
      rows.size
    );
    return {
      height: Math.min(
        height - columns.hasScrollbar * ScrollbarSize,
        rows.size + rows.crossSize
      ),
      width: Math.min(
        width - rows.hasScrollbar * ScrollbarSize,
        columns.size + columns.crossSize
      )
    };
  }
);
