import {
  KEY_SEPARATOR,
  AXIS_SEPARATOR,
  ROOT_ID,
  TOTAL_ID,
  HeaderType
} from "../constants";
import { isNullOrUndefined, isUndefined } from "./generic";

export function getHeaderSize(sizeAndPositionManager, index, span) {
  let res = 0;
  for (let i = 0; i < span; i += 1) {
    res += sizeAndPositionManager.getSizeAndPositionOfCell(index + i).size;
  }
  return res;
}

const findHeader = (headers, keys) => {
  if (keys.length === 1) {
    return headers.find(header => header.key === keys[0]);
  }
  const parentHeader = headers.find(header => header.key === keys[0]);
  if (!parentHeader) throw new Error("header not found");
  return findHeader(parentHeader.children, [
    [keys[0], keys[1]].join(KEY_SEPARATOR),
    ...keys.slice(2)
  ]);
};

export const keyToIndex = (headers, key) => {
  const keys = key.split(KEY_SEPARATOR);
  const depth = headers[0].length;
  const ancestorHeaders = headers
    .filter(headersRow => headersRow.length === depth)
    .map(headersRow => headersRow[0]);
  try {
    return findHeader(ancestorHeaders, keys).x;
  } catch (e) {
    // console.error(`Header with key ${key} not found in following headers`, headers);
    return -1;
  }
};

export const getLeaves = (node, acc = [], depth, notSorted) => {
  if (
    // isNullOrUndefined(node.orderedChildren) ||
    node.depth === (depth === undefined ? 10000 : depth) ||
    (node.id !== ROOT_ID && node.orderedChildren.length === 0)
  ) {
    acc.push(node);
    return acc;
  }
  if (notSorted) {
    Object.values(node.children).map(node => getLeaves(node, acc, depth));
  } else {
    node.orderedChildren.map(key => getLeaves(node.children[key], acc, depth));
  }
  return acc;
};

export function getKey({
  headerType,
  parent,
  measureId,
  crossAxisDimensionsCode,
  value,
  dimension
}) {
  switch (headerType) {
    case HeaderType.DATA_HEADER:
      if (parent.type === HeaderType.GRAND_TOTAL) {
        // If the parent is a Total header, split to include the measure id
        // on the right side of the axis separator
        const [totalID, crossAxisDimensionsCode] = parent.key.split(
          AXIS_SEPARATOR
        );
        return `${totalID}${KEY_SEPARATOR}${measureId}${AXIS_SEPARATOR}${crossAxisDimensionsCode}`;
      }
      return `${parent.key}${KEY_SEPARATOR}${measureId}`;
    case HeaderType.GRAND_TOTAL:
      return `${TOTAL_ID}${AXIS_SEPARATOR}${crossAxisDimensionsCode.join(
        KEY_SEPARATOR
      )}`;
    case HeaderType.SUB_TOTAL:
      return parent ? `${parent.key}${KEY_SEPARATOR}${value}` : value;
    case HeaderType.INNER:
    case HeaderType.WRAPPER:
      return parent
        ? `${parent.key}${KEY_SEPARATOR}${dimension.id}`
        : String(dimension.id);
    default:
      throw new Error(`Header type ${headerType} is unknown`);
  }
}

export function getNextKey(current, next) {
  const firstLeafHeader =
    current.firstHeaderRow[current.firstHeaderRow.length - 1];
  const keys = firstLeafHeader.key.split(KEY_SEPARATOR);
  let nextKey = "";
  if (current.dimensions.length > next.dimensions.length) {
    const nextDimensionIds = next.dimensions.map(dimension => dimension.id);
    const missingDimensionPosition = current.dimensions.findIndex(
      dimension => !nextDimensionIds.includes(dimension.id)
    );
    nextKey = keys.slice(0, missingDimensionPosition).join(KEY_SEPARATOR);
  } else if (current.dimensions.length < next.dimensions.length) {
    const previousDimensionIds = current.dimensions.map(
      dimension => dimension.id
    );
    const newDimensionPosition = next.dimensions.findIndex(
      dimension => !previousDimensionIds.includes(dimension.id)
    );
    nextKey = keys.slice(0, newDimensionPosition).join(KEY_SEPARATOR);
  } else if (current.dataDimensionsCount !== next.dataDimensionsCount) {
    // A data dimension has been toggled
    nextKey = keys.slice(0, -1).join(KEY_SEPARATOR);
  } else {
    // A filter has been modified
    // For the moment, do nothing
    nextKey = "";
  }
  return nextKey;
}

export function intersectDataIndexes(arg0, arg1) {
  if (isUndefined(arg0)) {
    return arg1;
  } else if (isUndefined(arg1)) {
    return arg0;
  } else {
    const n = arg0.length;
    const m = arg1.length;
    let i = 0;
    let j = 0;
    const res = [];
    while (i < n && j < m) {
      if (arg0[i] > arg1[j]) {
        j += 1;
      } else if (arg0[i] < arg1[j]) {
        i += 1;
      } else {
        res.push(arg0[i]);
        i += 1;
        j += 1;
      }
    }
    return res;
  }
}
