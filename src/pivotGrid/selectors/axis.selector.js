import { createSelector } from 'reselect';
import { Axis, AxisType, toAxisType } from '../Axis';
import { getFilteredData } from './data.selector';
import {
  rowDimensionsSelector,
  columnDimensionsSelector,
  activatedMeasuresSelector
} from './dimensions.selector';
import { getLeaves, countHeadersDepth, isNull } from '../utils/generic';
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
  [getFilteredData, rowDimensionsSelector, columnDimensionsSelector],
  (data, rows, columns) => buildAxisTrees(data, { columns, rows })
);
export const getColumnAxisTree = createSelector(
  [getAxisTrees],
  axisTrees => axisTrees.columns
);
export const getRowAxisTree = createSelector(
  [getAxisTrees],
  axisTrees => axisTrees.rows
);

///////////////////////////////////////////////////////////////////
// headers
//////////////////////////////////////////////////////////////////
// a=[{a:3,b:"toto 10"},{a:10,b:"toto 3"},{a:11,b:"toto 1"},{a:2,b:"toto 4"}]
function buildHeader(data, node, dimensions, measures, depth, parent) {
  const { id, children, dataIndexes } = node;
  let header;
  // Root node
  if (node.id === ROOT_ID) {
    header = { id: ROOT_ID };
  } else {
    const currentDimension = dimensions[depth];
    const row = data[dataIndexes[0]];
    header = {
      sortKey: currentDimension.sort.keyAccessor(row),
      id: currentDimension.keyAccessor(row),
      type: HeaderType.DIMENSION,
      parent,
      key: parent.id !== ROOT_ID ? `${parent.key}-/-${id}` : String(id),
      dataIndexes,
      orderedChildrenIds: []
    };
    header.hasSubTotal = currentDimension.hasSubTotal;
  }
  header.children = Object.keys(node.children).reduce(
    (acc, nodeId) => ({
      ...acc,
      [nodeId]: buildHeader(
        data,
        node.children[nodeId],
        dimensions,
        measures,
        depth + 1,
        header
      )
    }),
    {}
  );

  if (Object.keys(header.children).length > 0) {
    // sort children
    const orderedChildrenMap = Object.keys(header.children).map(id => ({
      id: header.children[id].id,
      sortKey: header.children[id].sortKey
    }));
    if (
      orderedChildrenMap.length > 0 &&
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
            orderedChildrenIds: []
          };
          header.orderedChildrenIds.push(id);
        });
      } else {
        header.children[EMPTY_ID] = {
          id: EMPTY_ID,
          type: HeaderType.MEASURE,
          key: `${header.key}-/-${EMPTY_ID}`,
          parent: header,
          dataIndexes: header.dataIndexes,
          orderedChildrenIds: []
        };
        header.orderedChildrenIds.push(EMPTY_ID);
      }
    }
  }
  return header;
}

export function buildAxisHeaders(data, axisTree, dimensions, measures) {
  return buildHeader(data, axisTree, dimensions, measures, -1, null);
}

export const getRowHeaders = createSelector(
  [
    getFilteredData,
    getRowAxisTree,
    rowDimensionsSelector,
    getAxisActivatedMeasures(AxisType.ROWS)
  ],
  buildAxisHeaders
);

export const getColumnHeaders = createSelector(
  [
    getFilteredData,
    getColumnAxisTree,
    columnDimensionsSelector,
    getAxisActivatedMeasures(AxisType.COLUMNS)
  ],
  buildAxisHeaders
);

export const getRowLeaves = createSelector([getRowHeaders], getLeaves);
export const getColumnLeaves = createSelector([getColumnHeaders], getLeaves);

export const getLayout = createSelector(
  [getRowHeaders, getColumnHeaders],
  (rowHeaders, columnHeaders) => {
    return {
      rowHorizontalCount: countHeadersDepth(rowHeaders),
      rowVerticalCount: getLeaves(rowHeaders).length,
      columnHorizontalCount: getLeaves(columnHeaders).length,
      columnVerticalCount: countHeadersDepth(columnHeaders)
    };
  }
);
