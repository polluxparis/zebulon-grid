import { createSelector } from "reselect";
import { Axis, AxisType, toAxisType } from "../Axis";
import AxisUi from "../AxisUi";
import { getFilteredData } from "./data.selector";
import {
  getRowDimensions,
  getColumnDimensions,
  getActivatedMeasures
} from "./dimensions.selector";

const getActivatedMeasuresCount = createSelector(
  [getActivatedMeasures],
  measures => measures.length
);

const getAxisActivatedMeasures = axisType =>
  createSelector(
    [getActivatedMeasures, state => state.config.dataHeadersLocation],
    (measures, dataHeadersLocation) => {
      if (toAxisType(dataHeadersLocation) === axisType) {
        return measures;
      }
      return null;
    }
  );

export const getRowUiAxis = createSelector(
  [
    getRowAxis,
    getAxisActivatedMeasures(AxisType.ROWS),
    state => state.axis.columns
  ],
  (rowAxis, activatedMeasures, crossDimensionsCode) =>
    new AxisUi(
      rowAxis,
      {
        activatedMeasures,
        activatedMeasuresCount: activatedMeasures ? activatedMeasures.length : 0
      },
      crossDimensionsCode
    )
);

export const getColumnUiAxis = createSelector(
  [
    getColumnAxis,
    getAxisActivatedMeasures(AxisType.COLUMNS),
    state => state.axis.rows
  ],
  (columnAxis, activatedMeasures, crossDimensionsCode) =>
    new AxisUi(
      columnAxis,
      {
        activatedMeasures,
        activatedMeasuresCount: activatedMeasures ? activatedMeasures.length : 0
      },
      crossDimensionsCode
    )
);

export const getLayout = createSelector(
  [
    getRowAxis,
    getColumnAxis,
    getRowUiAxis,
    getColumnUiAxis,
    getActivatedMeasuresCount,
    state => state.config.dataHeadersLocation
  ],
  (
    rowAxis,
    columnAxis,
    rowsUi,
    columnsUi,
    activatedMeasuresCount,
    dataHeadersLocation
  ) => {
    const rowHorizontalCount =
      (rowAxis.dimensions.length || 1) +
      (dataHeadersLocation === "rows" && activatedMeasuresCount >= 1 ? 1 : 0);
    const rowVerticalCount = rowsUi.headers.length;
    const columnHorizontalCount = columnsUi.headers.length;
    const columnVerticalCount =
      (columnAxis.dimensions.length || 1) +
      (dataHeadersLocation === "columns" && activatedMeasuresCount >= 1
        ? 1
        : 0);
    return {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount
    };
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
  const rowRoot = { id: null, children: {} };
  const columnRoot = { id: null, children: {} };
  // Create sorting accessors
  data.forEach((row, index) => {
    let columnNode = columnRoot;
    let rowNode = rowRoot;
    columns.forEach(dimension => {
      columnNode = buildNode(dimension.accessor(row), columnNode, index);
    });
    rows.forEach(dimension => {
      rowNode = buildNode(dimension.accessor(row), rowNode, index);
    });
  });
  return { columns: columnNode, rows: rowNode };
}
export const getAxisTrees = createSelector(
  [getFilteredData, getRowDimensions, getColumnDimensions],
  (data, columns, rows) => buildAxisTrees(data, { columns, rows })
);
export const getColumnAxisTree = createSelector(
  [getFilteredData, getColumnDimensions],
  buildAxisTree
);
export const getRowAxisTree = createSelector(
  [getFilteredData, getRowDimensions],
  buildAxisTree
);

///////////////////////////////////////////////////////////////////
// headers
//////////////////////////////////////////////////////////////////
function buildHeader(data, node, dimensions, measures, depth) {
  const { id, children, dataIndexes } = node;
  const currentDimension = dimensions[depth];
  const row = data[dataIndexes[0]];
  const header = {
    sortKey: currentDimension.sort.keyAccessor(row),
    id,
    dataIndexes,
    type: "dimension",
    children: Object.keys(node.children).reduce(
      (acc, nodeId) => ({
        ...acc,
        [nodeId]: buildHeader(
          node.children[nodeId],
          dimensions,
          dimensionValues,
          depth + 1
        )
      }),
      {}
    )
  };

  if (header.children.length === 0) {
    if (measures.length !== 0) {
      // measure headers
      measures.forEach((measure, index) => {
        header.children.push({
          id: measure.id,
          type: "measure"
        });
      });
    }
  } else {
    // sort children
    const mapOrder = Object.keys(header.children).map(id => ({
      id: id,
      sortKey: header.children[id].sortKey
    }));
    const sortFunction = (a, b) => header.sort.custom(a.sortKey, b.sortKey);
    mapOrder.sort(sortFunction);
    header.mapOrder = mapOrder.map(obj => obj.id);
    if (currentDimension.sort.direction === "desc") {
      header.mapOrder.reverse();
    }
    header.hasSubTotal = currentDimension.hasSubTotal;
  }
  // x value
  x += measures.length || 1;
  return header;
}

export function buildAxisHeaders(data, axisTree, dimensions, measures) {
  axisTree.children.map(node =>
    buildHeader(node, dimensions, measures, data, 0)
  );
}

export const getRowHeaders = createSelector(
  [getFilteredData, getRowAxisTree, getRowDimensions, getActivatedMeasures],
  buildAxisHeaders
);

export const getColumnHeaders = createSelector(
  [
    getFilteredData,
    getColumnAxisTree,
    getColumnDimensions,
    getActivatedMeasures
  ],
  buildAxisHeaders
);
