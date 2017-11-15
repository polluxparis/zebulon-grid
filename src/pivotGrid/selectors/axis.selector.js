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
import { getLeaves } from "../utils/headers";
import {
  ROOT_ID,
  TOTAL_ID,
  MEASURE_ID,
  HeaderType,
  AxisType,
  toAxis,
  toAxisType
} from "../constants";
import { filteredIndexesSelector } from "./data.selector";
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
  // console.log("buildAxisTrees0", x, data.length);
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

  // console.log("buildAxisTrees", data.length, Date.now() - x, rowRoot);
  return {
    columns: columnRoot,
    rows: rowRoot,
    dimensions,
    newRows,
    newColumns
  };
}
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

export const sort = (children, sortFunction, orderByAttribute) => {
  return children
    .sort(sortFunction)
    .map(
      child =>
        child.isAttribute && orderByAttribute
          ? child.attributeParentId
          : child.id
    );
};
const getFinalLeaves = (
  node,
  measures,
  measuresCount,
  dimension,
  filteredIndexes,
  leaves
) => {
  node.orderedChildren = [];
  // node.filteredIndexes = [];
  if (node.type === HeaderType.SUB_TOTAL) {
    if (node.id === ROOT_ID) {
      node.depth = 0;
      node.caption = "Grand total";
      node.isTotal = 2;
    }
    node.isVisible = !node.isParentCollapsed;
    node.nVisibles = node.isVisible * measuresCount;
  }
  // filtering
  // when a filter is set on one or several not diplayed dimensions=> filter data indexes
  // else filtering is made on leaves
  if (!node.isTotal && node.id !== ROOT_ID && filteredIndexes) {
    node.filteredIndexes = node.dataRowIndexes.filter(index =>
      filteredIndexes.get(index)
    );
  } else {
    node.filteredIndexes = node.dataRowIndexes;
  }
  if (node.filteredIndexes && !node.filteredIndexes.length) {
    return 0;
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
        isTotal: node.isTotal,
        isFiltered: node.isFiltered
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
  first,
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
  node.isVisible = (parent.isVisible && first) || !node.isParentCollapsed;
  node.isFiltered =
    !dimension.filter || dimension.filter[node.id] !== undefined;
  node.filteredIndexes = [];
  //  totals
  const totalKey = node.id === ROOT_ID ? toAxis(axis) + TOTAL_ID : dimension.id;
  if (
    nextDimension.id !== undefined &&
    subtotals[totalKey] &&
    !dimension.isAttribute &&
    node.isFiltered
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
      // push subtotals or grand total
      subtotalNode.nVisibles = getFinalLeaves(
        subtotalNode,
        measures,
        measuresCount,
        dimension,
        filteredIndexes,
        leaves
      );
    }
  }
  node.subtotal = subtotalNode;
  if (nextDimension.id !== undefined && node.isFiltered) {
    let children = Object.values(node.children),
      sortDimension = nextDimension;
    if (
      nextDimension.orderBy !== undefined &&
      nextDimension.orderBy !== nextDimension.depth
    ) {
      sortDimension = dimensions[nextDimension.orderBy];
      children = getLeaves(node, [], nextDimension.orderBy, true);
    }
    children = sort(
      children,
      sortDimension.sortFunction,
      sortDimension !== nextDimension
    );
    //  recursive loop on ordered children
    let first = true;
    children = children.map((id, index) => {
      const child = node.children[id];
      const nVisibles = getAxisLeaves(
        axis,
        child,
        dimensions,
        measures,
        measuresCount,
        areCollapsed,
        subtotals,
        totalsFirst,
        filteredIndexes,
        first,
        leaves
      );
      if (nVisibles) {
        first = false;
      }
      return {
        id,
        nVisibles,
        filteredIndexes: child.filteredIndexes,
        isFiltered: child.isFiltered
      };
    });
    // ! orders must include collapsed nodes but exclude nodes without child
    children = children.filter(
      child =>
        child.isFiltered &&
        (!nextDimension.filter || nextDimension.filter[child.id] !== undefined)
    );
    children = children.map(child => {
      nVisibles += child.nVisibles;
      // node.filteredIndexes.push(...child.filteredIndexes);
      node.filteredIndexes = node.filteredIndexes.concat(child.filteredIndexes);
      return child.id;
    });
    node.filteredIndexes.sort((a, b) => a - b);
    if (!node.orders) {
      node.orders = {};
    }
    node.orders[sortDimension.id] = children;
    node.orderedChildren = node.orders[sortDimension.id];
  }
  if (nextDimension.id === undefined && node.isFiltered) {
    // push terminal leaves or, if measures are on the axis,1 leave per measure
    nVisibles = getFinalLeaves(
      node,
      measures,
      measuresCount,
      dimension,
      filteredIndexes,
      leaves
    );
  }
  if (node.filteredIndexes && !node.filteredIndexes.length) {
    node.isVisible = false;
    node.nVisibles = 0;
    node.isFiltered = false;
  }
  if (subtotalNode && node.isVisible && !totalsFirst) {
    // push subtotals or grand total
    subtotalNode.nVisibles = getFinalLeaves(
      subtotalNode,
      measures,
      measuresCount,
      dimension,
      filteredIndexes,
      leaves
    );
  }
  if (subtotalNode) {
    subtotalNode.filteredIndexes = node.filteredIndexes;
    subtotalNode.isFiltered = node.isFiltered;
    if (!node.isFiltered) {
      subtotalNode.isVisible = false;
      subtotalNode.nVisibles = 0;
    }
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
    filteredIndexesSelector
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
          true,
          leaves
        );
      }

      return {
        nVisibles,
        node,
        leaves: totalsFirst ? leaves.filter(leaf => leaf.isFiltered) : leaves
      };
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
    filteredIndexesSelector
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
          true,
          leaves
        );
      }
      return {
        nVisibles,
        node,
        leaves: totalsFirst ? leaves.filter(leaf => leaf.isFiltered) : leaves
      };
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
  const children = Object.values(node.children || {}).filter(
    child => child.isFiltered
  );
  if (children.length) {
    let index = 0;
    let n = children.reduce((n, child) => {
      const nCells = child.dimensionId === MEASURE_ID ? measuresCount : 1;
      const isCollapsed =
        node.isParentCollapsed || (node.isCollapsed && !child.isAttribute);
      child.isVisible = (node.isVisible && index < nCells) || !isCollapsed;
      child.isCollapsed = child.isAttribute
        ? node.isCollapsed
        : child.isCollapsed;
      child.isParentCollapsed = isCollapsed;
      const n2 = childrenVisibles(child, measuresCount);
      if (n2) {
        index++;
      }
      if (child.subtotal) {
        child.subtotal.isVisible = !child.isParentCollapsed;
        child.subtotal.nVisibles = child.subtotal.isVisible * measuresCount;
        n += child.subtotal.nVisibles;
      }
      return n + n2;
    }, 0);
    return n;
  } else {
    node.nVisibles = 1;
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
const sortNode = (node, dimensionId, depth, sortFunction) => {
  let orders = node.orders[dimensionId];
  if (orders) {
    orders.reverse();
  } else {
    const orderedChildren = getLeaves(node, [], depth, true);
    orderedChildren.sort(sortFunction);
    orders = orderedChildren.map(
      child => (child.isAttribute ? child.attributeParentId : child.id)
    );
    node.orders[dimensionId] = orders;
  }
  node.orderedChildren = orders;
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
    const dimensions =
      axis === AxisType.ROWS ? rowDimensions : columnDimensions;
    const dimension = dimensions[depth];
    let sortingDepth = depth - 1;
    const sortFct = sortFunction(dimension);
    if (dimension.isAttribute) {
      while (dimensions[sortingDepth].id !== dimension.isAttributeOf) {
        sortingDepth--;
      }
      sortingDepth--;
    }
    dimensions[sortingDepth + 1].orderBy = dimension.depth;
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
        undefined,
        false,
        totalsFirst || false
      );
    }
  }
);
