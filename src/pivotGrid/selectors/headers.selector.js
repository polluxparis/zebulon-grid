///////////////////////////////////////////////////////////////////
//  compute the header trees (rows and columns) with positions ans sizes...
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";
import { isNull } from "../utils/generic";
import {
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
  // TOTAL_ID,
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
  areCollapsed,
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
    // header.indexes = { index, rootIndex: index, lastIndex: index };
    header.options = {
      isNotCollapsible:
        header.isAttribute ||
        header.depth >= getLastDimension(dimensions) ||
        (!header.isCollapsed && header.span === measuresCount),
      // isCollapsed: areCollapsed[header.key],
      isAffixManaged: false,
      isDropTarget: false
    };
    cellsKey[header.key] = true;
  } else {
    // if (!header.isCollapsed) {
    header.sizes.main.size += main.size;
    if (direction === -1) {
      header.sizes.main.position -= main.size;
      // header.isAffixManaged = true;
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
      areCollapsed,
      dimensions,
      measuresCount,
      direction
    );
    // header.indexes.rootIndex = header.parent.indexes.rootIndex;
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
    } else if (header.isCollapsed) {
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
export const buildPositionedHeaders = (
  leaves,
  headerSizes,
  crossPositions,
  containerSize,
  crossSize,
  dimensions,
  measures,
  areCollapsed,
  nVisibles,
  scroll
) => {
  console.log("buildPositionedHeaders0", Date.now(), scroll, nVisibles);
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
  // if (leaves[0].id === TOTAL_ID) {
  //   const header = leaves[0];
  //   header.key = TOTAL_ID;
  //   header.sizes = {
  //     main: { size: headerSizes(TOTAL_ID), position: 0 },
  //     cross: crossPositions[TOTAL_ID]
  //   };
  //   header.options = {
  //     isNotCollapsible: true,
  //     isCollapsed: false,
  //     isAffixManaged: false,
  //     isDropTarget: true
  //   };
  //   header.indexes = { index: 0, rootIndex: 0, lastIndex: 0 };
  //   cells.push([header]);
  //   size += header.sizes.main.size;
  // }
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
    if (dimension.isVisible && header.isVisible) {
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
        areCollapsed,
        dimensions,
        measuresCount,
        scroll.direction
      );
      header.options = {
        isNotCollapsible: true,
        isAffixManaged: false,
        isDropTarget:
          dimension.id === MEASURE_ID &&
          (header.depth === 0 || measuresCount > 1)
      };
      if (header.isParentCollapsed) {
        header.dataIndexes = header.parent.dataIndexes;
      } else {
        header.dataIndexes = header.dataRowIndexes;
      }
      lastSize = header.sizes.main.size;
      rowCells.push(header);
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
      leaves,
      headerSizes,
      crossPositions,
      containerSize,
      crossSize,
      dimensions,
      measures,
      areCollapsed,
      nVisibles,
      scroll
    );
  }
  // const offset = scroll.direction === 1 ? 0 : lastSize;
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
    startIndex:
      scroll.direction === 1 ? scroll.index : index - scroll.direction,
    stopIndex:
      scroll.direction === -1 ? scroll.index : index - scroll.direction,
    length: leaves.length,
    leaves,
    nVisibles,
    hasScrollbar: ix < nVisibles || size > maxSize,
    displayedRatio: (ix - 1) / nVisibles,
    positionRatio:
      (scroll.direction === 1 ? scroll.index : index) / leaves.length,
    scroll
  };
};

export const getRowHeadersSelector = createSelector(
  [
    rowLeavesSelector,
    getCellHeightByKeySelector,
    crossColumnPositionsSelector,
    columnHeadersHeightSelector,
    rowVisibleDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    state => state.collapses.rows,
    state => state.collapses.nVisibleRows,
    state => state.config.height
    // state => state.selectedRange.scrollToRow
  ],
  (
    leaves,
    headerSizes,
    crossPositions,
    crossSize,
    dimensions,
    measures,
    areCollapsed,
    nVisibles,
    containerSize,
    scroll
  ) => scroll => {
    return buildPositionedHeaders(
      leaves.leaves,
      headerSizes,
      crossPositions,
      containerSize,
      crossSize,
      dimensions,
      measures,
      areCollapsed,
      leaves.nVisibles + nVisibles,
      scroll
    );
  }
);
export const getColumnHeadersSelector = createSelector(
  [
    columnLeavesSelector,
    getCellWidthByKeySelector,
    crossRowPositionsSelector,
    rowHeadersWidthSelector,
    columnVisibleDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    state => state.collapses.columns,
    state => state.collapses.nVisibleColumns,
    state => state.config.width
    // state => state.selectedRange.scrollToColumn
  ],
  (
    leaves,
    headerSizes,
    crossPositions,
    crossSize,
    dimensions,
    measures,
    areCollapsed,
    nVisibles,
    containerSize,
    scroll
  ) => scroll =>
    buildPositionedHeaders(
      leaves.leaves,
      headerSizes,
      crossPositions,
      containerSize,
      crossSize,
      dimensions,
      measures,
      areCollapsed,
      leaves.nVisibles + nVisibles,
      scroll
    )
);
const combineRowsAndColumns = (rows, columns) => {
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
export const rowAndColumnHeadersSelector = createSelector(
  [
    getRowHeadersSelector,
    state => state.selectedRange.scrollToRow,
    getColumnHeadersSelector,
    state => state.selectedRange.scrollToColumn
  ],
  (getRows, scrollToRow, getColumns, scrollToColumn) => {
    let rows = getRows(scrollToRow);
    let columns = getColumns(scrollToColumn);
    combineRowsAndColumns(rows, columns);
    //  corrections of diplay aberations
    let change = false;
    if (
      columns.direction === -1 &&
      columns.cells[columns.cells.length - 1][0].sizes.main.position -
        columns.offset >
        0
    ) {
      scrollToColumn.index = 0;
      scrollToColumn.direction = 1;
      columns = getColumns(scrollToColumn);
      change = true;
    }
    if (
      rows.direction === -1 &&
      rows.cells[rows.cells.length - 1][0].sizes.main.position - rows.offset > 0
    ) {
      scrollToRow.index = 0;
      scrollToRow.direction = 1;
      rows = getRows(scrollToRow);
      change = true;
    }
    if (change) {
      combineRowsAndColumns(rows, columns);
    }
    return { rows, columns };
  }
);

export const getRowsCollapsedSelector = createSelector(
  [rowLeavesSelector, state => state.collapses.columns],
  (leaves, areCollapsed) => (maxSize, scroll) => {}
);
