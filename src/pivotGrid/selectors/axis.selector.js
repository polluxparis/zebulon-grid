import { createSelector } from 'reselect';
import { filteredDataSelector } from './data.selector';
import {
  rowDimensionsSelector,
  columnDimensionsSelector,
  activatedMeasuresSelector
} from './dimensions.selector';
import { isNull, isNullOrUndefined } from '../utils/generic';
import { getLeaves } from '../utils/headers';
import {
  ROOT_ID,
  EMPTY_ID,
  MEASURE_ID,
  HeaderType,
  AxisType,
  toAxisType
} from '../constants';

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
    node.children[id].dataIndexes.push(index);
    return node.children[id];
  } else {
    node.children[id] = { id, children: {}, dataIndexes: [index] };
    return node.children[id];
  }
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
function buildHeaders(
  data,
  node,
  dimensions,
  measures,
  depth,
  parent,
  areCollapsed
) {
  const { id, dataIndexes } = node;
  let header;
  // Root node
  if (node.id === ROOT_ID) {
    header = {
      id: ROOT_ID,
      type: HeaderType.GRAND_TOTAL,
      parent: null,
      dataIndexes: undefined,
      orderedChildrenIds: []
    };
  } else {
    const currentDimension = dimensions[depth];
    const row = data[dataIndexes[0]];
    const key = parent.id !== ROOT_ID ? `${parent.key}-/-${id}` : String(id);
    const isCollapsed = currentDimension.isAttribute
      ? parent.isCollapsed
      : areCollapsed[key] || false;
    header = {
      sortKey: currentDimension.sort.keyAccessor(row),
      id: currentDimension.keyAccessor(row),
      type: HeaderType.DIMENSION,
      parent,
      key,
      dataIndexes,
      orderedChildrenIds: [],
      isCollapsed,
      depth,
      span: 1
    };
    header.hasSubTotal = currentDimension.hasSubTotal;
  }
  if (
    !header.isCollapsed ||
    (depth + 1 < dimensions.length && dimensions[depth + 1].isAttribute)
  ) {
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
      header.children[orderedChildrenMap[0].id].type === HeaderType.DIMENSION
    ) {
      let childrenDimension = dimensions[depth + 1];

      if (!isNullOrUndefined(childrenDimension.sort.sortedBy)) {
        childrenDimension =
          dimensions[
            dimensions.findIndex(d => d.id === childrenDimension.sort.sortedBy)
          ] || childrenDimension;
      }
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
            isCollapsed: header.isCollapsed,
            span: 1,
            depth: dimensions.length - 1
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
          isCollapsed: header.isCollapsed,
          orderedChildrenIds: [],
          span: 1,
          depth: dimensions.length - 1
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
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    state => state.collapses.rows
  ],
  buildAxisHeaders
);

export const columnHeadersSelector = createSelector(
  [
    filteredDataSelector,
    columnAxisTreeSelector,
    columnDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    state => state.collapses.columns
  ],
  buildAxisHeaders
);

export const rowLeavesSelector = createSelector(
  [rowHeadersSelector],
  getLeaves
);
export const columnLeavesSelector = createSelector(
  [columnHeadersSelector],
  getLeaves
);

export const layoutSelector = createSelector(
  [
    rowLeavesSelector,
    columnLeavesSelector,
    rowDimensionsSelector,
    columnDimensionsSelector
  ],
  (rowLeaves, columnLeaves, rowDimensions, columnDimensions) => {
    return {
      rowHorizontalCount: rowDimensions.length,
      rowVerticalCount: rowLeaves.length,
      columnHorizontalCount: columnLeaves.length,
      columnVerticalCount: columnDimensions.length
    };
  }
);
export const getDimensionKeys = (
  node,
  dimensionDepth,
  depth,
  parent,
  isCollapsed
) => {
  const key = depth !== -1 && parent.id !== ROOT_ID
    ? `${parent.key}-/-${node.id}`
    : String(node.id);
  let keys = {};
  if (dimensionDepth === depth) {
    keys = { [key]: isCollapsed };
  } else {
    keys = Object.keys(node.children).reduce((acc, child) => {
      const k = getDimensionKeys(
        node.children[child],
        dimensionDepth,
        depth + 1,
        { ...node, key: key },
        isCollapsed
      );
      return { ...acc, ...k };
    }, {});
  }
  return keys;
};
export const getDimensionKeysSelector = createSelector(
  [rowAxisTreeSelector, columnAxisTreeSelector],
  (rowNode, columnNode) => (axis, depth, isCollapsed) =>
    getDimensionKeys(
      axis === AxisType.ROWS ? rowNode : columnNode,
      depth,
      -1,
      null,
      isCollapsed
    )
);
