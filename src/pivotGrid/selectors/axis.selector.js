///////////////////////////////////////////////////////////////////
//  compute the axis header trees (rows and columns)
///////////////////////////////////////////////////////////////////

import { createSelector } from "reselect";
import {
  rowVisibleDimensionsSelector,
  columnVisibleDimensionsSelector,
  dimensionsWithAxisSelector
} from "./dimensions.selector";
import { isNullOrUndefined } from "../utils/generic";
import { getLeaves, hasInter } from "../utils/headers";
import {
  ROOT_ID,
  TOTAL_ID,
  MEASURE_ID,
  HeaderType,
  AxisType,
  toAxis,
  toAxisType
} from "../constants";
import { filteredDataSelector } from "./data.selector";
export const getAxisActivatedMeasuresSelector = axisType =>
  createSelector(
    [
      state => state.measures,
      state => state.axis.measures,
      state => state.configuration.measureHeadersAxis
    ],
    (measures, axisMeasures, measureHeadersAxis) => {
      if (toAxisType(measureHeadersAxis) === axisType) {
        return axisMeasures.map(measure => measures[measure]);
      }
      return null;
    }
  );

///////////////////////////////////////////////////////////////////
// Axis trees
//////////////////////////////////////////////////////////////////
export const sortFunction = dimension => {
  if (dimension.sort.custom) {
    return (a, b) => dimension.sort.custom(a.sortKey, b.sortKey);
  } else {
    return (a, b) => (a.sortKey > b.sortKey) - (b.sortKey > a.sortKey);
  }
};
// add child node to a node
function buildNode(id, node, index, dimension) {
  if (node.children[id] === undefined) {
    const dimensionValue = dimension.values[id];
    node.children[id] = {
      id,
      dimensionId: dimension.id,
      type: HeaderType.DIMENSION,
      parent: node,
      children: dimension.last ? null : {},
      dataRowIndexes: dimension.isAttribute ? node.dataRowIndexes : [index],
      sortKey: dimensionValue.sortKey,
      caption: dimensionValue.caption,
      key: node.id === ROOT_ID ? `${id}` : `${node.key}-/-${id}`,
      depth: dimension.depth,
      isAttribute: dimension.isAttribute,
      attributeParentId: dimension.isAttribute
        ? isNullOrUndefined(node.attributeParentId)
          ? node.id
          : node.attributeParentId
        : null,
      isTotal: 0
    };
  } else if (!dimension.isAttribute) {
    node.children[id].dataRowIndexes.push(index);
  }
  return node.children[id];
}

// build colums and rows trees
export function buildAxisTrees(rowRoot, columnRoot, data, dimensions, offset) {
  if (data.length === 0) {
    return { columns: null, rows: null };
  }
  // Create sorting accessors
  const x = Date.now();
  console.log("buildAxisTrees0", x, data.length);
  let newValue = false,
    newRows = false,
    newColumns = false;
  data.forEach((row, index) => {
    let columnNode = columnRoot;
    let rowNode = rowRoot;
    dimensions.forEach(dimension => {
      if (dimension.id !== MEASURE_ID) {
        const id = dimension.keyAccessor(row);
        let dimensionValue = dimension.values[id];
        if (!dimensionValue) {
          dimensionValue = {
            id,
            dimensionId: dimension.id,
            caption: dimension.labelAccessor(row),
            sortKey: dimension.sort.keyAccessor(row),
            rowIndexes: [index],
            filtered: false
          };
          newValue = true;
          dimension.values[id] = dimensionValue;
        } else {
          dimension.values[id].rowIndexes.push(index);
        }

        if (dimension.axis === AxisType.ROWS) {
          newRows = newRows || newValue;
          rowNode = buildNode(id, rowNode, index + offset, dimension);
        } else if (dimension.axis === AxisType.COLUMNS) {
          newColumns = newColumns || newValue;
          columnNode = buildNode(id, columnNode, index + offset, dimension);
        }
        newValue = false;
      }
    });
  });

  console.log("buildAxisTrees", data.length, Date.now() - x, rowRoot);
  return {
    columns: columnRoot,
    rows: rowRoot,
    dimensions,
    newRows,
    newColumns
  };
}
// const mapAxis = (axisType, axises, dimensions) =>
//   axises.map((dimensionId, index) => {
//     const dimension = dimensions[dimensionId];
//     dimension.axis = axisType;
//     dimension.depth = index;
//     dimension.last = index === axises.length - 1;
//   });
export const getAxisTreesSelector = createSelector(
  [
    state => state.data.data,
    dimensionsWithAxisSelector,
    state => state.status.loadingConfig
  ],
  (data, dimensions, loadingConfig) => {
    if (!loadingConfig) {
      const rowRoot = {
        id: ROOT_ID,
        children: {},
        depth: -1,
        dimensionId: TOTAL_ID,
        type: HeaderType.MEASURE,
        key: TOTAL_ID,
        caption: "Total"
      };
      const columnRoot = { ...rowRoot, children: {} };
      return buildAxisTrees(
        rowRoot,
        columnRoot,
        data,
        dimensions.map(dimension => {
          dimension.values = {};
          dimension.sortFunction = sortFunction(dimension);
          return dimension;
        }),
        0
      );
    } else {
      return {};
    }
  }
);
export const columnAxisTreeSelector = createSelector(
  [getAxisTreesSelector],
  axisTrees => axisTrees.columns
);
export const rowAxisTreeSelector = createSelector(
  [getAxisTreesSelector],
  axisTrees => axisTrees.rows
);

export const sortAndFilter = (children, sortFunction, filter) => {
  let nodes = Object.values(children);
  if (filter) {
    nodes = nodes.filter(node => filter[node.id] !== undefined);
  }
  return nodes.sort(sortFunction).map(child => child.id);
};
const getFinalLeaves = (
  node,
  measures,
  measuresCount,
  // filteredIndexes,
  leaves
) => {
  node.orderedChildren = [];
  if (node.id === ROOT_ID && node.type === HeaderType.SUB_TOTAL) {
    node.depth = 0;
    node.caption = "Grand total";
    node.isTotal = 2;
  }
  if (measures) {
    node.children = {};
    measures.forEach((measure, index) => {
      node.children[measure.id] = {
        id: measure.id,
        dimensionId: MEASURE_ID,
        type: HeaderType.MEASURE,
        parent: node,
        children: {},
        orderedChildren: [],
        filteredIndexes: node.filteredIndexes,
        caption: measure.caption,
        key: `${node.key}-/-${measure.id}`,
        depth: node.depth + 1,
        isAttribute: false,
        isVisible: node.isVisible,
        nVisibles: node.isVisible + 0,
        isTotal: node.isTotal
      };
      node.orderedChildren.push(measure.id);
      leaves.push(node.children[measure.id]);
    });
  } else if (node.id === ROOT_ID && node.type !== HeaderType.SUB_TOTAL) {
    leaves.push({ ...node, depth: 0, parent: node });
    node.orderedChildren.push(node.id);
  } else {
    leaves.push(node);
  }
  return node.isVisible * measuresCount;
};
export const getAxisLeaves = (
  axis,
  node,
  dimensions,
  measures,
  measuresCount,
  areCollapsed,
  subtotals,
  totalsFirst,
  filteredIndexes,
  index,
  leaves = []
) => {
  if (!node) {
    return 0;
  }
  let nVisibles = 0,
    subtotalNode = null;
  // last dimension
  // add measures if needed
  const dimension = dimensions[node.depth] || {};
  const nextDimension = dimensions[node.depth + 1] || {};

  const isAttribute = node.depth !== -1 ? dimension.isAttribute : false;
  const parent = node.parent || { isVisible: true };
  node.isCollapsed =
    areCollapsed[node.key] || (parent.isCollapsed && isAttribute);
  node.isParentCollapsed =
    parent.isParentCollapsed || (parent.isCollapsed && !isAttribute);
  node.isVisible = (parent.isVisible && index === 0) || !node.isParentCollapsed;
  // when a filter is set on one or several not diplayed dimensions=> filter data indexes
  if (node.id !== ROOT_ID && filteredIndexes) {
    node.filteredIndexes = node.dataRowIndexes.filter(index =>
      filteredIndexes.get(index)
    );
    if (node.filteredIndexes.length === 0) {
      node.isVisible = false;
      node.nVisibles = 0;
      return 0;
    }
  } else {
    node.filteredIndexes = node.dataRowIndexes;
  }
  //  totals
  const totalKey = node.id === ROOT_ID ? toAxis(axis) + TOTAL_ID : dimension.id;
  if (
    nextDimension.id !== undefined &&
    subtotals[totalKey] &&
    !dimension.isAttribute
  ) {
    subtotalNode = {
      ...node,
      isCollapsed: true,
      type: HeaderType.SUB_TOTAL,
      key: `${TOTAL_ID}-/-${node.key}`,
      caption: "Total " + node.caption,
      children: [],
      orderedChildren: [],
      isTotal: 1
    };
    if (totalsFirst) {
      subtotalNode.nVisibles = getFinalLeaves(
        subtotalNode,
        measures,
        measuresCount,
        // filteredIndexes,
        leaves
      );
    }
  }
  node.subtotal = subtotalNode;

  if (nextDimension.id !== undefined) {
    node.orders = {
      [nextDimension.id]: sortAndFilter(
        node.children,
        nextDimension.sortFunction,
        nextDimension.filter
      )
    };
    node.orderedChildren = node.orders[nextDimension.id];
    node.orderedChildren.forEach((key, index) => {
      nVisibles += getAxisLeaves(
        axis,
        node.children[key],
        dimensions,
        measures,
        measuresCount,
        areCollapsed,
        subtotals,
        totalsFirst,
        filteredIndexes,
        index,
        leaves
      );
    });
  }
  if (nextDimension.id === undefined) {
    nVisibles = getFinalLeaves(
      node,
      measures,
      measuresCount,
      // filteredIndexes,
      leaves
    );
  } else if (subtotalNode && !totalsFirst && node.nVisibles) {
    subtotalNode.nVisibles = getFinalLeaves(
      subtotalNode,
      measures,
      measuresCount,
      // filteredIndexes,
      leaves
    );
  }
  node.nVisibles = nVisibles;
  return nVisibles + (subtotalNode !== null ? subtotalNode.nVisibles : 0);
};
export const rowLeavesSelector = createSelector(
  [
    rowAxisTreeSelector,
    dimensionsWithAxisSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    state => state.collapses.configurationRows,
    state => state.filters,
    state => state.subtotals,
    state => state.configuration.totalsFirst,
    state => state.status.loadingConfig,
    filteredDataSelector
  ],
  (
    node,
    dimensions,
    measures,
    areCollapsed,
    filters,
    subtotals,
    totalsFirst,
    loadingConfig,
    filteredIndexes
  ) => {
    if (!loadingConfig) {
      const x = Date.now();
      const leaves = [];
      let nVisibles;
      if (node) {
        const axisDimensions = dimensions
          .filter(dimension => dimension.axis === AxisType.ROWS)
          .map(dimension => {
            dimension.filter = (filters[dimension.id] || {
              values: null
            }).values;
            return dimension;
          });
        nVisibles = getAxisLeaves(
          AxisType.ROWS,
          node,
          axisDimensions,
          measures,
          measures === null ? 1 : measures.length,
          areCollapsed,
          subtotals,
          totalsFirst,
          filteredIndexes,
          0,
          leaves
        );
      }
      console.log(
        "rowLeavesSelector",
        leaves.length,
        nVisibles,
        Date.now() - x,
        node,
        leaves
      );
      return { nVisibles, node, leaves };
    } else {
      return {};
    }
  }
);

export const columnLeavesSelector = createSelector(
  [
    columnAxisTreeSelector,
    dimensionsWithAxisSelector,
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS),
    state => state.collapses.configurationColumns,
    state => state.filters,
    state => state.subtotals,
    state => state.configuration.totalsFirst,
    state => state.status.loadingConfig,
    filteredDataSelector
  ],
  (
    node,
    dimensions,
    measures,
    areCollapsed,
    filters,
    subtotals,
    totalsFirst,
    loadingConfig,
    filteredIndexes
  ) => {
    if (!loadingConfig) {
      const x = Date.now();
      const leaves = [];
      let nVisibles;
      if (node) {
        const axisDimensions = dimensions
          .filter(dimension => dimension.axis === AxisType.COLUMNS)
          .map(dimension => {
            dimension.filter = (filters[dimension.id] || {
              values: null
            }).values;
            return dimension;
          });
        nVisibles = getAxisLeaves(
          AxisType.COLUMNS,
          node,
          axisDimensions,
          measures,
          measures === null ? 1 : measures.length,
          areCollapsed,
          subtotals,
          totalsFirst,
          filteredIndexes,
          0,
          leaves
        );
      }
      console.log(
        "columnLeavesSelector",
        leaves.length,
        nVisibles,
        Date.now() - x,
        node,
        leaves
      );
      return { nVisibles, node, leaves };
    } else {
      return {};
    }
  }
);
const parentsVisibles = (node, nVisibles) => {
  node.nVisibles += nVisibles;
  if (node.id !== ROOT_ID) {
    parentsVisibles(node.parent, nVisibles);
  }
};
const childrenVisibles = (node, measuresCount) => {
  const children = Object.values(node.children || {});
  if (children.length) {
    let n = children.reduce((n, child, index) => {
      const nCells = child.dimensionId === MEASURE_ID ? measuresCount : 1;
      const isCollapsed =
        node.isParentCollapsed || (node.isCollapsed && !child.isAttribute);
      child.isVisible = (node.isVisible && index < nCells) || !isCollapsed;
      child.isCollapsed = child.isAttribute
        ? node.isCollapsed
        : child.isCollapsed;
      child.isParentCollapsed = isCollapsed;
      n += childrenVisibles(child, measuresCount);
      return n;
    }, 0);
    if (node.subtotal) {
      node.subtotal.isVisible = node.isVisible && !node.isParentCollapsed;
      n += childrenVisibles(node.subtotal, measuresCount);
    }
    return n;
  } else {
    return node.isVisible + 0;
  }
};
export const expandCollapseNode = (node, isCollapsed, measuresCount) => {
  const { nVisibles } = node;
  // mutate the headers
  if (isCollapsed !== undefined && isCollapsed === node.isCollapsed) {
    return 0;
  }
  node.isCollapsed =
    isCollapsed === undefined ? !node.isCollapsed : isCollapsed;
  node.nVisibles = childrenVisibles(node, measuresCount);
  parentsVisibles(node.parent, node.nVisibles - nVisibles);
  return node.nVisibles - nVisibles;
};

const expandCollapseAll = (node, depth, isCollapsed, measuresCount) => {
  const leaves = getLeaves(node, [], depth);
  const keys = leaves.reduce(
    (acc, leaf) => {
      acc.n += expandCollapseNode(leaf, isCollapsed, measuresCount);
      acc.keys[leaf.key] = isCollapsed;
      return acc;
    },
    { n: 0, keys: {} }
  );
  return keys;
};

export const getExpandCollapseKeysSelector = createSelector(
  [
    rowAxisTreeSelector,
    columnAxisTreeSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS),
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS)
  ],
  (rowAxisTree, columnAxisTree, rowMeasures, columnMeasures) => (
    axisType,
    depth,
    isCollapsed
  ) => {
    const measures = axisType === AxisType.ROWS ? rowMeasures : columnMeasures;
    return expandCollapseAll(
      axisType === AxisType.ROWS ? rowAxisTree : columnAxisTree,
      depth,
      isCollapsed,
      measures === null ? 1 : measures.length
    );
  }
);
const sortNode = (node, attributeId, attributeDepth, sortFunction) => {
  if (attributeId) {
    if (node.orders[attributeId]) {
      node.orders[attributeId].reverse();
    } else {
      const orderedChildren = getLeaves(node, [], attributeDepth, true);
      orderedChildren.sort(sortFunction);
      node.orders[attributeId] = orderedChildren.map(
        child => child.attributeParentId
      );
    }
    node.orderedChildren = node.orders[attributeId];
  } else {
    node.orderedChildren.reverse();
  }
};
export const toggleSortOrderSelector = createSelector(
  [
    rowLeavesSelector,
    columnLeavesSelector,
    rowVisibleDimensionsSelector,
    columnVisibleDimensionsSelector,
    state => state.configuration.totalsFirst
  ],
  (rowLeaves, columnLeaves, rowDimensions, columnDimensions, totalsFirst) => (
    axis,
    depth
  ) => {
    let sortFct;
    const dimensions =
      axis === AxisType.ROWS ? rowDimensions : columnDimensions;
    const dimension = dimensions[depth];
    let sortingDepth = depth - 1;
    if (dimension.isAttribute) {
      while (dimensions[sortingDepth].id !== dimension.isAttributeOf) {
        sortingDepth--;
      }
      sortFct = sortFunction(dimension);
      sortingDepth--;
      if (true) {
      }
    }

    if (dimension.id !== TOTAL_ID && dimension.id !== MEASURE_ID) {
      const leaves = axis === AxisType.ROWS ? rowLeaves : columnLeaves;
      const sort = dimension.sort;
      if (sort.direction === "desc") {
        sort.direction = "asc";
      } else {
        sort.direction = "desc";
      }
      const sortedLeaves = getLeaves(leaves.node, [], sortingDepth, true);
      sortedLeaves.map(node => sortNode(node, dimension.id, depth, sortFct));
      leaves.leaves = getLeaves(
        leaves.node,
        [],
        null,
        false,
        totalsFirst || false
      );
    }
  }
);
