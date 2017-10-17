///////////////////////////////////////////////////////////////////
//  compute the header trees (rows and columns) with positions ans sizes...
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";
import { isNull } from "../utils/generic";
import {
  filteredDataSelector,
  filteredPushedDataSelector
} from "./data.selector";

import {
  buildAxisTrees,
  buildHeadersTree,
  rowLeavesSelector,
  columnLeavesSelector,
  getAxisActivatedMeasuresSelector,
  // rowHeadersTreeSelector,
  // columnHeadersTreeSelector,
  sortFunction,
  buildAxisLeaves
} from "./axis.selector2";
import {
  rowDimensionsSelector,
  columnDimensionsSelector,
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
        header.span === measuresCount,
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
    } else {
      // if (header.isCollapsed)
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
  headers,
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
  // console.log("buildPositionedHeaders0", Date.now(), scroll, nVisibles);
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
      leaves,
      headers,
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
    headers,
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
    // rowLeavesSelector,
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
    // leaves,
    headerSizes,
    crossPositions,
    crossSize,
    dimensions,
    measures,
    areCollapsed,
    nVisibles,
    containerSize,
    scroll
  ) => (leaves, scroll) => {
    return buildPositionedHeaders(
      leaves.leaves,
      leaves.headers,
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
    // columnLeavesSelector,
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
    // leaves,
    headerSizes,
    crossPositions,
    crossSize,
    dimensions,
    measures,
    areCollapsed,
    nVisibles,
    containerSize,
    scroll
  ) => (leaves, scroll) =>
    buildPositionedHeaders(
      leaves.leaves,
      leaves.headers,
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
const mergeHeader = (header, newHeader, dimensions, dataCount) => {
  //   console.log("merge", header, newHeader);
  if (header.dataRowIndexes) {
    header.dataRowIndexes.push(
      ...newHeader.dataRowIndexes //.map(x => x + dataCount)
    );
  }
  const newKeys = Object.keys(newHeader.keys);
  let bNew = false,
    bNewChild = false;
  newKeys.map(key => {
    const index = header.keys[key];
    const newIndex = newHeader.keys[key];
    if (index === undefined) {
      // console.log("new", newHeader.children[newIndex]);
      bNewChild = true;
      header.keys[key] = header.children.length;
      header.children.push(newHeader.children[newIndex]);
    } else {
      const { span, nVisibles } = header.children[index];
      const child = mergeHeader(
        header.children[index],
        newHeader.children[newIndex],
        dimensions,
        dataCount
      );
      // console.log("changed", child, header.children[index]);
      header.span += child.header.span - span;
      header.nVisibles += child.header.nVisibles - nVisibles;
      header.children[index] = child.header;
      bNew = bNew || child.bNew;
    }
  });
  if (bNewChild) {
    // reorder
    // A voir attribute orders
    header.span = 0;
    header.nVisibles = 0;
    if (header.children[0].type === HeaderType.DIMENSION) {
      const childrenDimension = dimensions[header.depth + 1];
      const orderedChildren = header.children.map((child, index) => {
        header.span += child.span;
        header.nVisibles += child.nVisibles;
        return {
          index,
          sortKey: child.sortKey
        };
      });

      orderedChildren.sort(sortFunction(childrenDimension));
      header.orders = {
        [childrenDimension.id]: orderedChildren.map(obj => obj.index)
      };
      header.orderedChildren = header.orders[childrenDimension.id];
      if (childrenDimension.sort.direction === "desc") {
        header.orderedChildren.reverse();
      }
    }
  }
  //   const orders=Object.keys(header.orders);
  //   orders.map(dimensionId=>{
  //       const dimension=dimensions[dimensionId];
  //       .sort(sortFunction(dimension));

  //   });
  //    orderedChildren.sort(sortFunction(childrenDimension));
  //     header.orders = {
  //       [childrenDimension.id]: orderedChildren.map(obj => obj.index)
  //     };
  //     header.orderedChildren = header.orders[childrenDimension.id];
  //     if (childrenDimension.sort.direction === "desc") {
  //       header.orderedChildren.reverse();
  //     }
  // }
  return { bNew: bNew || bNewChild, header };
};
export const pushedDataSelector = createSelector(
  [
    filteredPushedDataSelector,
    filteredDataSelector,
    rowDimensionsSelector,
    columnDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    rowLeavesSelector,
    columnLeavesSelector,
    state => state.collapses,
    state => state.filters,
    state => state.dimensions
  ],
  (
    filteredPushedData,
    filteredData,
    rowDimensions,
    columnDimensions,
    rowMeasures,
    columnMeasures,
    rowLeaves,
    columnLeaves,
    collapses,
    filters,
    dimensions
  ) => {
    // const filteredPushedData = getFilteredPushedData(data, filters, dimensions);
    const dataCount = filteredData.length - filteredPushedData.length;
    if (!filteredPushedData || !filteredPushedData.length) {
      return { rowLeaves, columnLeaves };
    }
    const newAxisTrees = buildAxisTrees(
      filteredPushedData,
      rowDimensions,
      columnDimensions,
      dataCount
    );
    const newRowHeaders = buildHeadersTree(
      filteredPushedData,
      newAxisTrees.rows,
      rowDimensions,
      rowMeasures,
      isNull(rowMeasures) + 0 || Object.keys(rowMeasures).length,
      collapses.rows,
      0,
      -1,
      null,
      dataCount
    );
    const newColumnHeaders = buildHeadersTree(
      filteredPushedData,
      newAxisTrees.columns,
      columnDimensions,
      columnMeasures,
      isNull(columnMeasures) + 0 || Object.keys(columnMeasures).length,
      collapses.columns,
      0,
      -1,
      null,
      dataCount
    );
    // rowLeaves.bNew = mergeHeader(
    //   rowLeaves.headers,
    //   newRowHeaders,
    //   rowDimensions,
    //   dataCount
    // ).bNew;
    // columnLeaves.bNew = mergeHeader(
    //   columnLeaves.headers,
    //   newColumnHeaders,
    //   columnDimensions,
    //   dataCount
    // ).bNew;
    if (
      mergeHeader(rowLeaves.headers, newRowHeaders, rowDimensions, dataCount)
        .bNew
    ) {
      rowLeaves = buildAxisLeaves(rowLeaves.headers);
      rowLeaves.bNew = true;
    }
    if (
      mergeHeader(
        columnLeaves.headers,
        newColumnHeaders,
        columnDimensions,
        dataCount
      ).bNew
    ) {
      columnLeaves = buildAxisLeaves(columnLeaves.headers);
      columnLeaves.bNew = true;
    }
    return { rowLeaves, columnLeaves };
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
  (newHeaders, getRows, scrollToRow, getColumns, scrollToColumn) => {
    // console.log("newHeaders", newHeaders);
    if (newHeaders.rowLeaves.bNew) {
      scrollToRow.refreshLeaves = !scrollToRow.refreshLeaves;
    }
    if (newHeaders.columnLeaves.bNew) {
      scrollToColumn.refreshLeaves = !scrollToColumn.refreshLeaves;
    }
    let rows = getRows(newHeaders.rowLeaves, scrollToRow);
    let columns = getColumns(newHeaders.columnLeaves, scrollToColumn);

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
