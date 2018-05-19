///////////////////////////////////////////////////////////////////
//  compute the header trees (rows and columns) with positions ans sizes...
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";
import { utils } from "zebulon-controls";

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
  filteredIndexesSelector,
  notVisibleFiltersSelector,
  dataFilteredIndexes
} from "./data.selector";
import {
  ROOT_ID,
  TOTAL_ID,
  MEASURE_ID,
  ScrollbarSize,
  AxisType,
  HeaderType
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
  direction,
  isCollapsibleAttribute
) => {
  if (!header || (header.id === ROOT_ID && !header.isTotal)) {
    return null;
  }
  // et main = mainSizes;
  const first = cellsKey[header.key] === undefined;
  if (first) {
    const dimension = dimensions[header.depth];
    header.format = dimension.format;
    header.sizes = {
      main: { ...main },
      cross: { ...crossPositions[dimension.id] }
    };
    header.options = {
      isCollapsibleAttribute:
        header.isAttribute &&
        header.depth < getLastDimension(dimensions) &&
        (isCollapsibleAttribute || header.orderedChildren.length > 1),
      isNotCollapsible:
        (header.isAttribute ||
          header.depth >= getLastDimension(dimensions) ||
          header.orderedChildren.length < 2) &&
        !isCollapsibleAttribute,
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
  if (
    header.parent &&
    (header.parent.id !== ROOT_ID || header.parent.isTotal)
  ) {
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
      direction,
      header.options.isCollapsibleAttribute
    );
  }
  if (first) {
    if (
      dimensions[header.depth].isVisible &&
      (!header.isParentCollapsed || header.parent.id === ROOT_ID)
    ) {
      rowCells.push(header);
    }
    if (
      header.isCollapsed &&
      (header.type === HeaderType.GRAND_TOTAL ||
        header.type === HeaderType.SUB_TOTAL)
    ) {
      header.sizes.cross.collapsed =
        crossPositions[MEASURE_ID].position - header.sizes.cross.size;
      header.sizes.cross.size = crossPositions[MEASURE_ID].position;
    } else if (header.isCollapsed) {
      header.sizes.cross.collapsed = collapsedSizes[header.depth];
      header.sizes.cross.size += collapsedSizes[header.depth];
    }
    if (header.isParentCollapsed) {
      header.dataIndexes = header.parent.dataIndexes;
    } else {
      header.dataIndexes = header.filteredIndexes;
    }
  }
  return header.rootIndex;
};

const getLastDimension = dimensions =>
  dimensions.length -
  1 -
  ((dimensions[dimensions.length - 1] || {}).id === MEASURE_ID);
const getCrossHeadersSize = (dimensions, crossPositions) => {
  const lastDimension = getLastDimension(dimensions);
  if (lastDimension === -1) {
    return 0;
  }
  const last = crossPositions[dimensions[lastDimension].id];
  return last.position + last.size;
};
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
  // if (!leaves.length) {
  //   return undefined;
  // }
  // const x = Date.now();
  const maxSize = containerSize - crossSize;
  let size = 0,
    depth = 0;
  const measuresCount = utils.isNull(measures)
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
  } else if (scroll.index === -1 && leaves.length) {
    scroll.index = 0;
    scroll.direction = 1;
  }
  let index = scroll.index;
  let ix = 0;
  let lastSize;
  const crossHeadersSize = getCrossHeadersSize(dimensions, crossPositions);
  while (size <= maxSize && index < leaves.length && index >= 0) {
    const header = leaves[index];
    const dimension = dimensions[header.depth];
    header.format = dimension.format || (({ value }) => value);
    header.index = index;
    if (header.isVisible) {
      const rowCells = [];
      const mainSize = headerSizes(header.key);
      header.sizes = {
        main: {
          size: mainSize,
          position: scroll.direction === 1 ? size : maxSize - size - mainSize
        },
        cross: { ...crossPositions[dimension.id] }
      };
      // grand total and subtotals
      if (
        header.isCollapsed
        // ||
        // (header.dimensionId === MEASURE_ID &&
        //   (header.parent.type === HeaderType.GRAND_TOTAL ||
        //     header.parent.type === HeaderType.SUB_TOTAL))
      ) {
        header.sizes.cross.collapsed =
          crossHeadersSize -
          header.sizes.cross.position -
          header.sizes.cross.size;
        header.sizes.cross.size =
          crossHeadersSize - header.sizes.cross.position;
      } else if (
        header.dimensionId === MEASURE_ID &&
        (header.parent.type === HeaderType.GRAND_TOTAL ||
          header.parent.type === HeaderType.SUB_TOTAL)
      ) {
        header.sizes.cross.position = crossHeadersSize;
        // console.log("sizes", header.sizes);
      }
      parentsSizes(
        header.parent,
        header.sizes.main,
        crossPositions,
        collapsedSizes,
        scroll.direction === 1 ? false : size + mainSize > maxSize,
        cellsKey,
        rowCells,
        dimensions,
        measuresCount,
        scroll.direction,
        false
      );
      if (header.isTotal && header.dimensionId === MEASURE_ID) {
        header.sizes.cross.position += collapsedSizes[header.parent.depth];
      }
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
        header.dataIndexes = header.filteredIndexes;
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
  index -= scroll.direction;
  // index = Math.min(Math.max(index, 0), leaves.length - 1);
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
  // console.log("buildPositionedHeaders", Date.now() - x, scroll, cells);
  const hasScrollbar = ix < nVisibles || size > maxSize;
  if (measuresCount > 1) {
    // console.log(
    //   "selector",
    //   scroll.direction,
    //   (scroll.direction === 1 ? scroll.index : index + (size > maxSize)) /
    //     leaves.length
    // );
  }
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
    // incompleteIndex=size===Math.min(maxSize, size)
    length: leaves.length,
    leaves,
    // headers,
    nVisibles,
    hasScrollbar,
    // displayedRatio: (ix - (size > maxSize)) / nVisibles,
    displayedRatio:
      cells.length - (size > maxSize) === nVisibles
        ? maxSize / Math.min(maxSize, size)
        : (cells.length - (size > maxSize)) / nVisibles,
    positionRatio:
      (scroll.direction === 1 ? scroll.index : index + (size > maxSize)) /
      leaves.length,
    scroll
  };
};

export const getRowHeadersSelector = createSelector(
  [
    getCellHeightByKeySelector,
    crossColumnPositionsSelector,
    state => state.configuration.height,
    columnHeadersHeightSelector,
    rowVisibleDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    state => state.status.refreshDisplay
  ],
  (
    headerSizes,
    crossPositions,
    containerSize,
    crossSize,
    dimensions,
    measures,
    forceRefresh
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
    state => state.configuration.width,
    rowHeadersWidthSelector,
    columnVisibleDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    state => state.status.refreshDisplay
  ],
  (
    headerSizes,
    crossPositions,
    containerSize,
    crossSize,
    dimensions,
    measures,
    forceRefresh
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
  // console.log(rows, columns);
  if (
    (rows.hasScrollbar && !columns.hasScrollbar) ||
    (!rows.hasScrollbar && columns.hasScrollbar)
  ) {
    if (
      rows.hasScrollbar &&
      columns.size + ScrollbarSize > columns.containerSize - columns.crossSize
    ) {
      columns.hasScrollbar = true;
    } else if (
      columns.hasScrollbar &&
      rows.size + ScrollbarSize > rows.containerSize - rows.crossSize
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
    state => state.collapses,
    state => state.subtotals,
    state => state.configuration.totalsFirst,
    filteredIndexesSelector,
    notVisibleFiltersSelector
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
    collapses,
    subtotals,
    totalsFirst,
    filteredIndexes,
    filters
  ) => {
    if (!data.pushedData.length) {
      return { rows: rowLeaves, columns: columnLeaves };
    }
    // build the axis trees only for new data using the previous root
    // nodes of the intial root will be completed
    const newAxisTrees = buildAxisTrees(
      axisTrees.rows,
      axisTrees.columns,
      data.pushedData,
      axisTrees.dimensions,
      data.data.length
    );
    // filtered indexes must be completed when filters are applied on not displayed dimensions
    if (filters.length) {
      dataFilteredIndexes(
        data,
        filters,
        dimensions,
        filteredIndexes,
        data.data.length
      );
    }
    // when new dimension key appears, axis leaves must be rebuild
    let leaves, axisDimensions;
    if (newAxisTrees.newRows) {
      leaves = [];
      axisDimensions = axises.rows.map(axis => dimensions[axis]);
      getAxisLeaves(
        AxisType.ROWS,
        axisTrees.rows,
        axisDimensions,
        rowMeasures,
        rowMeasures === null ? 1 : rowMeasures.length,
        collapses.rows,
        subtotals,
        totalsFirst,
        filteredIndexes,
        true,
        leaves
      );
      rowLeaves.leaves = leaves;
    }
    // when new dimension key appears, axis leaves must be rebuild
    if (newAxisTrees.newColumns) {
      leaves = [];
      axisDimensions = axises.columns.map(axis => dimensions[axis]);
      getAxisLeaves(
        AxisType.COLUMNS,
        axisTrees.columns,
        axisDimensions,
        columnMeasures,
        columnMeasures === null ? 1 : columnMeasures.length,
        collapses.columns,
        subtotals,
        totalsFirst,
        filteredIndexes,
        true,
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
    state => state.selectedRange.scrollToColumn,
    state => state.status.loadingConfig
  ],
  (
    rowAndColumnLeaves,
    getRowHeaders,
    scrollToRow,
    getColumnHeaders,
    scrollToColumn,
    loadingConfig
  ) => {
    let rows, columns;
    console.log("rowAndColumnHeadersSelector");
    if (
      !(
        loadingConfig ||
        utils.isNullOrUndefined(rowAndColumnLeaves.rows.node) ||
        utils.isNullOrUndefined(rowAndColumnLeaves.columns.node)
      )
    ) {
      rows = getRowHeaders(rowAndColumnLeaves.rows, scrollToRow);
      columns = getColumnHeaders(rowAndColumnLeaves.columns, scrollToColumn);
      if (utils.isNullOrUndefined(rows) || utils.isNullOrUndefined(rows)) {
        return { rows, columns };
      }
      combineRowsAndColumns(rows, columns);
      // corrections of display aberations
      let change = false;
      if (
        columns.direction === -1 &&
        columns.cells.length &&
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
        rows.cells.length &&
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
    state => state.configuration.height,
    state => state.configuration.width,
    rowAndColumnHeadersSelector
  ],
  (height, width, rowsAndColumns) => {
    const { rows, columns } = rowsAndColumns;
    // console.log(
    //   height,
    //   width,
    //   columns.crossSize,
    //   columns.size,
    //   rows.crossSize,
    //   rows.size
    // );
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
