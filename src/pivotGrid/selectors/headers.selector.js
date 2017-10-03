///////////////////////////////////////////////////////////////////
//  compute the header trees (rows and columns) with positions ans sizes...
///////////////////////////////////////////////////////////////////
import { createSelector } from "reselect";
import { filteredDataSelector } from "./data.selector";
import { isNull } from "../utils/generic";
import {
  rowLeavesSelector,
  columnLeavesSelector,
  getAxisActivatedMeasuresSelector
} from "./axis.selector";
import {
  rowDimensionsSelector,
  columnDimensionsSelector
} from "./dimensions.selector";

import {
  crossPositionsSelector,
  getCellHeightByKeySelector,
  getCellWidthByKeySelector
} from "./cellSizes.selector";

import {
  ROOT_ID,
  TOTAL_ID,
  MEASURE_ID,
  HeaderType,
  AxisType
} from "../constants";

///////////////////////////////////////////////////////////////////
// headers
//////////////////////////////////////////////////////////////////
const parentsSizes = (header, main, index) => {
  if (header.parent.id !== ROOT_ID) {
    if (header.parent.index === undefined || header.parent.index === index) {
      header.parent.main = { size: main.size, position: main.position };
      header.parent.index = index;
      header.parent.rootIndex = index;
    } else {
      header.parent.main.size += main.size;
    }
    header.parent.lastIndex = index;
    header.rootIndex = parentsSizes(header.parent, main, index);
  } else {
    header.rootIndex = header.index;
  }
  return header.rootIndex;
};
export function buildAxisPositionsHeaders(
  data,
  leaves,
  headerSizes,
  crossPositions,
  dimensions,
  measures,
  axisType
) {
  console.log("buildAxisPositionsHeaders0", Date.now());
  let totalSize = 0,
    depth = 0,
    crossSize = 0,
    totalCrossSize = 0;
  const lastNotMeasureDimensionIndex =
    dimensions.length - 1 - !isNull(measures);
  const measuresCount = isNull(measures)
    ? 1
    : Object.keys(measures).length || 1;

  // pre compute colllapses size
  const collapsedSizes = [];
  let collapsedSize = 0,
    nextDimensionIsAttribute,
    size;
  for (let index = lastNotMeasureDimensionIndex; index >= 0; index -= 1) {
    if (nextDimensionIsAttribute) {
      collapsedSizes.push(0);
    } else {
      collapsedSizes.push(collapsedSize);
    }
    nextDimensionIsAttribute = dimensions[index].isAttribute;
    size = crossPositions[dimensions[index].id].size; // headerSizes({ index: [index] });
    collapsedSize += size;
  }
  collapsedSizes.reverse();
  // no dimension is on the axis ==> Total header
  let leafKey,
    renderedHeaderKeys = {};
  const headerCells = [];
  if (leaves[0].id === ROOT_ID) {
    const header = leaves[0];
    header.key = TOTAL_ID;
    header.main = { size: headerSizes(ROOT_ID), position: 0 }; // sizeAndPositionManager.getSizeAndPositionOfCell(0);
    header.cross = crossPositions[ROOT_ID];
    header.index = 0;
    header.rootIndex = 0;
    header.lastIndex = 0;
    const headerCell = {
      header,
      caption: "Total",
      dimensionId: ROOT_ID,
      isNotCollapsible: true,
      isCollapsed: false,
      isAffixManaged: false,
      isDropTarget: false
    };
    headerCells.push([headerCell]);
    totalSize += header.main.size;
    crossSize = header.cross.size;
  }
  // Loop on leaves
  let offset = 0;

  leaves.map((header, index) => {
    // header = { ...leaf };
    leafKey = header.key;
    // Loop on leaf and parents
    while (header.id !== ROOT_ID && !renderedHeaderKeys[header.key]) {
      renderedHeaderKeys[header.key] = true;
      const dimension = dimensions[header.depth];
      let caption;
      if (header.type === HeaderType.DIMENSION) {
        caption = dimension.format(
          dimension.labelAccessor(data[header.dataIndexes[0]])
        );
      } else {
        caption = measures[header.id].caption;
      }
      let isAffixManaged = true;
      if (leafKey === header.key) {
        header.main = { size: headerSizes(header.key), position: offset };
        header.rootIndex = parentsSizes(header, header.main, index);
        offset += header.main.size;
        header.index = index;
        header.lastIndex = index;
        isAffixManaged = false;
        totalSize += header.main.size;
        totalCrossSize = Math.max(totalCrossSize, crossSize);
        crossSize = 0;
      }
      header.cross = { ...crossPositions[dimension.id] };
      crossSize += header.cross.size;
      if (header.isCollapsed && collapsedSizes.length > 0) {
        if (header.type !== HeaderType.MEASURE) {
          header.cross.collapsed = collapsedSizes[header.depth];
          header.cross.size += collapsedSizes[header.depth];
        }
      }
      const isNotCollapsible =
        header.type === HeaderType.MEASURE ||
        dimension.isAttribute ||
        header.depth >= lastNotMeasureDimensionIndex ||
        (!header.isCollapsed && header.span === measuresCount);
      const isDropTarget =
        dimension.id === MEASURE_ID &&
        (header.depth === 0 || measuresCount > 1);
      const headerCell = {
        header,
        caption,
        dimensionId: dimension.id,
        isNotCollapsible,
        isCollapsed: header.isCollapsed,
        isAffixManaged,
        isDropTarget: isDropTarget
      };
      depth = Math.max(depth, header.depth);
      if (!headerCells[index]) {
        headerCells.push([headerCell]);
      } else {
        headerCells[index].push(headerCell);
      }
      header = header.parent;
    }
  });
  console.log("buildAxisPositionsHeaders1", Date.now(), headerCells.length);

  return {
    size: totalSize,
    crossSize: totalCrossSize,
    depth: depth + 1,
    headers: headerCells
  };
}

export const rowHeadersPositionsAndSizesSelector = createSelector(
  [
    filteredDataSelector,
    rowLeavesSelector,
    getCellHeightByKeySelector,
    crossPositionsSelector,
    rowDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.ROWS)
  ],
  (data, leaves, headerSizes, crossPositions, dimensions, measures) =>
    buildAxisPositionsHeaders(
      data,
      leaves,
      headerSizes,
      crossPositions[AxisType.ROWS],
      dimensions,
      measures,
      AxisType.ROWS
    )
);
export const columnHeadersPositionsAndSizesSelector = createSelector(
  [
    filteredDataSelector,
    columnLeavesSelector,
    getCellWidthByKeySelector,
    crossPositionsSelector,
    columnDimensionsSelector,
    getAxisActivatedMeasuresSelector(AxisType.COLUMNS)
  ],
  (data, leaves, headerSizes, crossPositions, dimensions, measures) =>
    buildAxisPositionsHeaders(
      data,
      leaves,
      headerSizes,
      crossPositions[AxisType.COLUMNS],
      dimensions,
      measures,
      AxisType.COLUMNS
    )
);
