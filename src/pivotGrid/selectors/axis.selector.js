///////////////////////////////////////////////////////////////////
//  compute the axis header trees (rows and columns)
///////////////////////////////////////////////////////////////////

import { createSelector } from "reselect";
import { filteredDataSelector } from "./data.selector";
import {
  rowDimensionsSelector,
  columnDimensionsSelector,
  activatedMeasuresSelector
} from "./dimensions.selector";
import { isNull, isNullOrUndefined } from "../utils/generic";
import { getLeaves } from "../utils/headers";
import {
  ROOT_ID,
  TOTAL_ID,
  MEASURE_ID,
  HeaderType,
  AxisType,
  toAxisType
} from "../constants";

export const getAxisActivatedMeasuresSelector = axisType =>
  createSelector(
    [activatedMeasuresSelector, state => state.config.measureHeadersAxis],
    (measures, measureHeadersAxis) => {
      if (toAxisType(measureHeadersAxis) === axisType) {
        return measures;
      }
      return null;
    }
  );

///////////////////////////////////////////////////////////////////
// Axis trees
//////////////////////////////////////////////////////////////////

// add child node to a node
function buildNode(id, node, index) {
  if (node.children[id] !== undefined) {
    node.children[id].dataRowIndexes.push(index);
    return node.children[id];
  } else {
    node.children[id] = { id, children: {}, dataRowIndexes: [index] };
    return node.children[id];
  }
}

// build colums and rows trees
function buildAxisTrees(data, { columns, rows }) {
  const rowRoot = { id: ROOT_ID, children: {} };
  const columnRoot = { id: ROOT_ID, children: {} };
  // Create sorting accessors
  console.log("buildNode0", Date.now());

  data.forEach((row, index) => {
    let columnNode = columnRoot;
    let rowNode = rowRoot;
    columns.forEach(dimension => {
      if (dimension.id !== MEASURE_ID) {
        columnNode = buildNode(dimension.keyAccessor(row), columnNode, index);
      }
    });
    rows.forEach(dimension => {
      if (dimension.id !== MEASURE_ID) {
        rowNode = buildNode(dimension.keyAccessor(row), rowNode, index);
      }
    });
  });
  console.log("buildNode", Date.now());
  return { columns: columnRoot, rows: rowRoot };
}
const getAxisTreesSelector = createSelector(
  [filteredDataSelector, rowDimensionsSelector, columnDimensionsSelector],
  (data, rows, columns) => buildAxisTrees(data, { columns, rows })
);
export const columnAxisTreeSelector = createSelector(
  [getAxisTreesSelector],
  axisTrees => axisTrees.columns
);
export const rowAxisTreeSelector = createSelector(
  [getAxisTreesSelector],
  axisTrees => axisTrees.rows
);

///////////////////////////////////////////////////////////////////
// headers
//////////////////////////////////////////////////////////////////

function buildHeadersTree(
  data,
  node,
  dimensions,
  measures,
  measuresCount,
  areCollapsed,
  index,
  depth,
  parent
) {
  const { id, dataRowIndexes } = node;
  if (depth === -1) {
    console.log("buildHeadersTree0", Date.now());
  }
  let header;
  // Root node
  if (node.id === ROOT_ID) {
    header = {
      id: ROOT_ID,
      dimensionId: ROOT_ID,
      type: HeaderType.GRAND_TOTAL,
      caption: "Total",
      parent: null,
      dataRowIndexes: undefined,
      children: [],
      orderedChildren: [],
      nVisibles: 0,
      options: {},
      isCollapsed: false,
      isParentCollapsed: false,
      isVisible: true,
      isAttribute: false,
      depth: -1
    };
  } else {
    const currentDimension = dimensions[depth];
    const row = data[dataRowIndexes[0]];
    const key = parent.id !== ROOT_ID ? `${parent.key}-/-${id}` : String(id);
    const isCollapsed =
      areCollapsed[key] || (parent.isCollapsed && currentDimension.isAttribute);
    const isParentCollapsed =
      parent.isParentCollapsed ||
      (parent.isCollapsed && !currentDimension.isAttribute);
    const isVisible = (parent.isVisible && index === 0) || !isParentCollapsed;
    // const isCollapsed = parent.isCollapsed||areCollapsed[key]

    header = {
      sortKey: currentDimension.sort.keyAccessor(row),
      id: currentDimension.keyAccessor(row),
      dimensionId: currentDimension.id,
      type: HeaderType.DIMENSION,
      caption: currentDimension.format(currentDimension.labelAccessor(row)),
      parent,
      key,
      dataRowIndexes,
      children: [],
      orderedChildren: [],
      depth,
      span: 1,
      nVisibles: 0,
      isCollapsed,
      isParentCollapsed,
      isAttribute: currentDimension.isAttribute,
      isVisible
    };
  }
  // recursion on children
  header.children = Object.values(node.children).map((node, index) =>
    buildHeadersTree(
      data,
      node,
      dimensions,
      measures,
      measuresCount,
      areCollapsed,
      index,
      depth + 1,
      header
    )
  );
  // level management after recursion
  if (header.children.length > 0) {
    // sort children and count span
    header.span = 0;
    const orderedChildren = header.children.map((child, index) => {
      header.span += child.span;
      header.nVisibles += child.nVisibles;
      return {
        index,
        sortKey: child.sortKey
      };
    });
    // count visible headers
    if (header.isCollapsed) {
      header.nVisibles = measuresCount;
    }
    // sorting using child dimension description
    if (header.children[0].type === HeaderType.DIMENSION) {
      let childrenDimension = dimensions[depth + 1];
      if (!isNullOrUndefined(childrenDimension.sort.sortedBy)) {
        childrenDimension = dimensions[header.depth + 1];
      }
      let sortFunction;
      if (childrenDimension.sort.custom) {
        sortFunction = (a, b) =>
          childrenDimension.sort.custom(a.sortKey, b.sortKey);
      } else {
        sortFunction = (a, b) =>
          (a.sortKey > b.sortKey) - (b.sortKey > a.sortKey);
      }
      orderedChildren.sort(sortFunction);
      header.orderedChildren = orderedChildren.map(obj => obj.index);
      if (childrenDimension.sort.direction === "desc") {
        header.orderedChildren.reverse();
      }
    }
  } else {
    header.nVisibles = header.isVisible * measuresCount;
    if (!isNull(measures)) {
      const measureIds = Object.keys(measures);
      if (measureIds.length > 0) {
        // measure headers
        header.children = [];
        header.orderedChildren = [];
        measureIds.forEach((id, index) => {
          header.children.push({
            id: id,
            type: HeaderType.MEASURE,
            dimensionId: MEASURE_ID,
            caption: measures[id].caption,
            parent: header,
            dataRowIndexes: header.dataRowIndexes,
            key: `${header.key}-/-${id}`,
            children: [],
            orderedChildren: [],
            span: 1,
            depth: dimensions.length - 1,
            isVisible: header.isVisible,
            nVisibles: header.isVisible || 0,
            isAttribute: false
          });
          header.span = measureIds.length;
          header.orderedChildren.push(index);
        });
      }
    } else {
      if (header.id === ROOT_ID) {
        header.children = [
          {
            id: TOTAL_ID,
            dimensionId: TOTAL_ID,
            type: HeaderType.MEASURE,
            key: TOTAL_ID,
            caption: "Total",
            parent: header,
            dataRowIndexes: header.dataRowIndexes,
            children: [],
            orderedChildren: [],
            span: 1,
            depth: 0,
            isVisible: true,
            nVisibles: 1,
            isAttribute: false
          }
        ];
        header.orderedChildren = [0];
      }
    }
    // }
  }
  if (depth === -1) {
    console.log("buildHeadersTree", Date.now());
  }
  return header;
}

export const rowHeadersTreeSelector = createSelector(
  [
    filteredDataSelector,
    rowAxisTreeSelector,
    rowDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    state => state.collapses.configRows
  ],
  (data, axisTree, dimensions, measures, areCollapsed) =>
    buildHeadersTree(
      data,
      axisTree,
      dimensions,
      measures,
      isNull(measures) || Object.keys(measures).length,
      areCollapsed,
      0,
      -1,
      null
    )
);

export const columnHeadersTreeSelector = createSelector(
  [
    filteredDataSelector,
    columnAxisTreeSelector,
    columnDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    state => state.collapses.configColumns
  ],
  (data, axisTree, dimensions, measures, areCollapsed) =>
    buildHeadersTree(
      data,
      axisTree,
      dimensions,
      measures,
      isNull(measures) || Object.keys(measures).length,
      areCollapsed,
      0,
      -1,
      null
    )
);
export function buildAxisLeaves(headers) {
  console.log("buildLeaves0", Date.now());
  const leaves = getLeaves(headers, []);
  console.log("buildLeaves", Date.now(), leaves.length);
  return { nVisibles: headers.nVisibles, leaves };
}

export const rowLeavesSelector = createSelector(
  [rowHeadersTreeSelector, state => state.status.toRefreshLeaves.rows],
  buildAxisLeaves
);

export const columnLeavesSelector = createSelector(
  [columnHeadersTreeSelector, state => state.status.toRefreshLeaves.columns],
  buildAxisLeaves
);
const headersParentVisibles = (header, nVisibles) => {
  header.nVisibles += nVisibles;
  if (header.id !== ROOT_ID) {
    headersParentVisibles(header.parent, nVisibles);
  }
};
const headersChildrenVisibles = (header, measuresCount) => {
  if (header.children.length) {
    return header.children.reduce((n, child, index) => {
      const nCells = child.dimensionId === MEASURE_ID ? measuresCount : 1;
      const isCollapsed =
        header.isParentCollapsed || (header.isCollapsed && !child.isAttribute);
      child.isVisible = (header.isVisible && index < nCells) || !isCollapsed;
      child.isCollapsed = child.isAttribute
        ? header.isCollapsed
        : child.isCollapsed;
      child.isParentCollapsed = isCollapsed;
      n += headersChildrenVisibles(child, measuresCount);
      return n;
    }, 0);
  } else {
    return header.isVisible;
  }
};
export const expandCollapseHeader = (header, isCollapsed, measuresCount) => {
  const { nVisibles } = header;
  // mutate the headers
  if (isCollapsed !== undefined && isCollapsed === header.isCollapsed) {
    return 0;
  }
  header.isCollapsed =
    isCollapsed === undefined ? !header.isCollapsed : isCollapsed;
  header.nVisibles = headersChildrenVisibles(header, measuresCount);
  headersParentVisibles(header.parent, header.nVisibles - nVisibles);
  return header.nVisibles - nVisibles;
};

const expandCollapseAll = (header, depth, isCollapsed, measureCount) => {
  // console.log(header, depth);
  const headers = getLeaves(header, [], depth);
  const keys = headers.reduce(
    (acc, header) => {
      acc.n += expandCollapseHeader(header, isCollapsed);
      acc.keys[header.key] = isCollapsed;
      return acc;
    },
    { n: 0, keys: {} }
  );
  // leaves.collapses += keys.n;
  return keys;
};

export const getExpandCollapseKeysSelector = createSelector(
  [
    rowHeadersTreeSelector,
    columnHeadersTreeSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS)
  ],
  (rowHeaders, columnHeaders, rowMeasures, columnMeasures) => (
    axisType,
    depth,
    isCollapsed
  ) => {
    const measures = AxisType.ROWS ? rowMeasures : columnMeasures;
    return expandCollapseAll(
      axisType === AxisType.ROWS ? rowHeaders : columnHeaders,
      depth,
      isCollapsed,
      measures === null ? 1 : measures.length
    );
  }
);
export const toggleSortOrderSelector = createSelector(
  [
    rowHeadersTreeSelector,
    columnHeadersTreeSelector,
    rowDimensionsSelector,
    columnDimensionsSelector
  ],
  (rowHeader, columnHeader, rowDimensions, columnDimensions) => (
    axis,
    depth
  ) => {
    const dimension =
      axis === AxisType.ROWS ? rowDimensions[depth] : columnDimensions[depth];
    if (dimension.id !== TOTAL_ID && dimension.id !== MEASURE_ID) {
      console.log(axis, depth, rowDimensions, columnDimensions);
      const header = axis === AxisType.ROWS ? rowHeader : columnHeader;
      const sort = dimension.sort;
      if (sort.direction === "desc") {
        sort.direction = "asc";
      } else {
        sort.direction = "desc";
      }
      const leaves = getLeaves(header, [], depth - 1);
      leaves.map(header => header.orderedChildren.reverse());
    }
  }
);
