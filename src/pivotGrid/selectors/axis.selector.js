import { createSelector } from 'reselect';
import { Axis, AxisType, toAxisType } from '../Axis';
import { filteredDataSelector } from './data.selector';
import {
  rowDimensionsSelector,
  columnDimensionsSelector,
  activatedMeasuresSelector
} from './dimensions.selector';
import {
  getLeaves,
  countHeadersDepth,
  isNull,
  isNullOrUndefined
} from '../utils/generic';
import { ROOT_ID, EMPTY_ID, MEASURE_ID } from '../constants';
import { HeaderType } from '../Cells';

const activatedMeasuresSelectorCount = createSelector(
  [activatedMeasuresSelector],
  measures => measures.length
);

export const getAxisActivatedMeasures = axisType =>
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
    node.children[id].dataIndexes.push(index);
    return node.children[id];
  } else {
    node.children[id] = { id, children: {}, dataIndexes: [index] };
    return node.children[id];
  }
}
// build one axis (colums or rows) tree
function buildAxisTree(data, dimensions) {
  return buildAxisTrees(data, { columns: dimensions, rows: [] }).columns;
}

// build colums and rows trees
function buildAxisTrees(data, { columns, rows }) {
  const rowRoot = { id: ROOT_ID, children: {} };
  const columnRoot = { id: ROOT_ID, children: {} };
  // Create sorting accessors
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
  return { columns: columnRoot, rows: rowRoot };
}
export const getAxisTrees = createSelector(
  [filteredDataSelector, rowDimensionsSelector, columnDimensionsSelector],
  (data, rows, columns) => buildAxisTrees(data, { columns, rows })
);
export const columnAxisTreeSelector = createSelector(
  [getAxisTrees],
  axisTrees => axisTrees.columns
);
export const rowAxisTreeSelector = createSelector(
  [getAxisTrees],
  axisTrees => axisTrees.rows
);

///////////////////////////////////////////////////////////////////
// headers
//////////////////////////////////////////////////////////////////
// a=[{a:3,b:"toto 10"},{a:10,b:"toto 3"},{a:11,b:"toto 1"},{a:2,b:"toto 4"}]
function buildHeaders(
  data,
  node,
  dimensions,
  measures,
  depth,
  parent,
  areCollapsed
) {
  const { id, children, dataIndexes } = node;
  let header;
  // Root node
  if (node.id === ROOT_ID) {
    header = { id: ROOT_ID };
  } else {
    const currentDimension = dimensions[depth];
    const row = data[dataIndexes[0]];
    const key = parent.id !== ROOT_ID ? `${parent.key}-/-${id}` : String(id);
    header = {
      sortKey: currentDimension.sort.keyAccessor(row),
      id: currentDimension.keyAccessor(row),
      type: HeaderType.DIMENSION,
      parent,
      key,
      dataIndexes,
      orderedChildrenIds: [],
      isCollapsed: areCollapsed[key],
      depth,
      span: 1
      // hasCollapsedParent: parent.isCollapsed || parent.hasCollapsedParent
    };
    header.hasSubTotal = currentDimension.hasSubTotal;
  }
  if (!header.isCollapsed) {
    header.children = Object.keys(node.children).reduce(
      (acc, nodeId) => ({
        ...acc,
        [nodeId]: buildHeaders(
          data,
          node.children[nodeId],
          dimensions,
          measures,
          depth + 1,
          header,
          areCollapsed
        )
      }),
      {}
    );
  } else {
    header.children = {};
  }
  const childrenKeys = Object.keys(header.children);
  if (childrenKeys.length > 0) {
    // sort children and count span
    header.span = 0;
    const orderedChildrenMap = childrenKeys.map(id => {
      header.span += header.children[id].span;
      return {
        id: header.children[id].id,
        sortKey: header.children[id].sortKey
      };
    });
    if (
      // orderedChildrenMap.length > 0 &&
      header.children[orderedChildrenMap[0].id].type === HeaderType.DIMENSION
    ) {
      const childrenDimension = dimensions[depth + 1];
      let sortFunction;
      if (childrenDimension.sort.custom) {
        sortFunction = (a, b) =>
          childrenDimension.sort.custom(a.sortKey, b.sortKey);
      } else {
        sortFunction = (a, b) =>
          (a.sortKey > b.sortKey) - (b.sortKey > a.sortKey);
      }
      orderedChildrenMap.sort(sortFunction);
      header.orderedChildrenIds = orderedChildrenMap.map(obj => obj.id);
      if (childrenDimension.sort.direction === 'desc') {
        header.orderedChildrenIds.reverse();
      }
    }
  } else {
    if (!isNull(measures)) {
      const measureIds = Object.keys(measures);
      if (measureIds.length > 0) {
        // measure headers
        measureIds.forEach((id, index) => {
          header.children[id] = {
            id: id,
            type: HeaderType.MEASURE,
            parent: header,
            dataIndexes: header.dataIndexes,
            key: `${header.key}-/-${id}`,
            orderedChildrenIds: [],
            span: 1,
            depth: depth + 1
          };
          header.span = measureIds.length;
          header.orderedChildrenIds.push(id);
        });
      } else {
        header.children[EMPTY_ID] = {
          id: EMPTY_ID,
          type: HeaderType.MEASURE,
          key: `${header.key}-/-${EMPTY_ID}`,
          parent: header,
          dataIndexes: header.dataIndexes,
          orderedChildrenIds: [],
          span: 1,
          depth: depth + 1
        };
        header.orderedChildrenIds.push(EMPTY_ID);
      }
    }
  }
  return header;
}

export function buildAxisHeaders(
  data,
  axisTree,
  dimensions,
  measures,
  areCollapsed
) {
  return buildHeaders(
    data,
    axisTree,
    dimensions,
    measures,
    -1,
    null,
    areCollapsed
  );
}

export const rowHeadersSelector = createSelector(
  [
    filteredDataSelector,
    rowAxisTreeSelector,
    rowDimensionsSelector,
    getAxisActivatedMeasures(AxisType.ROWS),
    state => state.collapses.rows
  ],
  buildAxisHeaders
);

export const columnHeadersSelector = createSelector(
  [
    filteredDataSelector,
    columnAxisTreeSelector,
    columnDimensionsSelector,
    getAxisActivatedMeasures(AxisType.COLUMNS),
    state => state.collapses.columns
  ],
  buildAxisHeaders
);

export const rowLeavesSelector = createSelector(
  [rowHeadersSelector],
  getLeaves
);
export const getColumnLeaves = createSelector(
  [columnHeadersSelector],
  getLeaves
);

export const getLayout = createSelector(
  [
    rowHeadersSelector,
    columnHeadersSelector,
    rowDimensionsSelector,
    columnDimensionsSelector
  ],
  (rowHeaders, columnHeaders, rowDimensions, columnDimensions) => {
    return {
      rowHorizontalCount: rowDimensions.length,
      rowVerticalCount: getLeaves(rowHeaders).length,
      columnHorizontalCount: getLeaves(columnHeaders).length,
      columnVerticalCount: columnDimensions.length
    };
  }
);
