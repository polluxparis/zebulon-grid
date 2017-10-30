import { KEY_SEPARATOR, ROOT_ID } from "../constants";
import { isUndefined } from "./generic";

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

export const getLeaves = (node, acc = [], depth, notSorted, totalsFirst) => {
  if (
    // isNullOrUndefined(node.orderedChildren) ||
    node.depth === (depth === undefined ? 10000 : depth) ||
    (node.id !== ROOT_ID &&
      depth === undefined &&
      node.orderedChildren.length === 0)
  ) {
    acc.push(node);
    return acc;
  }
  if (notSorted) {
    Object.values(node.children)
      .filter(node => node.isFiltered)
      .map(node => getLeaves(node, acc, depth, notSorted));
  } else {
    if (totalsFirst && node.subtotal) {
      getLeaves(node.subtotal, acc);
    }
    node.orderedChildren.map(key =>
      getLeaves(node.children[key], acc, depth, notSorted, totalsFirst)
    );
    if (totalsFirst === false && node.subtotal) {
      getLeaves(node.subtotal, acc);
    }
  }
  return acc;
};
export const resetLeaves = node => {
  Object.values(node.children).map(child => resetLeaves(child));
  if (node.subtotal) {
    resetLeaves(node.subtotal);
  }
  node.children = null;
  node.subtotal = null;
  node.dataRowIndexes = null;
  node.orderedChildren = null;
  node.filteredIndexes = null;
  node.dataIndexes = null;
  if (node.orders) {
    Object.keys(node.orders).map(key => (node.orders[key] = null));
  }
  node.orders = null;
  node.parent = null;
  node.options = null;
  node.sizes = null;
  node = undefined;
  return null;
};

export const resetDimensions = dimensions =>
  Object.values(dimensions).map(dimension => {
    Object.values(dimension.values).map(value => {
      value.rowIndexes = null;
      value = null;
    });
    dimension = null;
  });
//   headerType,
//   parent,
//   measureId,
//   crossAxisDimensionsCode,
//   value,
//   dimension
// }) {
//   switch (headerType) {
//     case HeaderType.DATA_HEADER:
//       if (parent.type === HeaderType.GRAND_TOTAL) {
//         // If the parent is a Total header, split to include the measure id
//         // on the right side of the axis separator
//         const [totalID, crossAxisDimensionsCode] = parent.key.split(
//           AXIS_SEPARATOR
//         );
//         return `${totalID}${KEY_SEPARATOR}${measureId}${AXIS_SEPARATOR}${crossAxisDimensionsCode}`;
//       }
//       return `${parent.key}${KEY_SEPARATOR}${measureId}`;
//     case HeaderType.GRAND_TOTAL:
//       return `${TOTAL_ID}${AXIS_SEPARATOR}${crossAxisDimensionsCode.join(
//         KEY_SEPARATOR
//       )}`;
//     case HeaderType.SUB_TOTAL:
//       return parent ? `${parent.key}${KEY_SEPARATOR}${value}` : value;
//     case HeaderType.INNER:
//     case HeaderType.WRAPPER:
//       return parent
//         ? `${parent.key}${KEY_SEPARATOR}${dimension.id}`
//         : String(dimension.id);
//     default:
//       throw new Error(`Header type ${headerType} is unknown`);
//   }
// }

// export function getNextKey(current, next) {
//   const firstLeafHeader =
//     current.firstHeaderRow[current.firstHeaderRow.length - 1];
//   const keys = firstLeafHeader.key.split(KEY_SEPARATOR);
//   let nextKey = "";
//   if (current.dimensions.length > next.dimensions.length) {
//     const nextDimensionIds = next.dimensions.map(dimension => dimension.id);
//     const missingDimensionPosition = current.dimensions.findIndex(
//       dimension => !nextDimensionIds.includes(dimension.id)
//     );
//     nextKey = keys.slice(0, missingDimensionPosition).join(KEY_SEPARATOR);
//   } else if (current.dimensions.length < next.dimensions.length) {
//     const previousDimensionIds = current.dimensions.map(
//       dimension => dimension.id
//     );
//     const newDimensionPosition = next.dimensions.findIndex(
//       dimension => !previousDimensionIds.includes(dimension.id)
//     );
//     nextKey = keys.slice(0, newDimensionPosition).join(KEY_SEPARATOR);
//   } else if (current.dataDimensionsCount !== next.dataDimensionsCount) {
//     // A data dimension has been toggled
//     nextKey = keys.slice(0, -1).join(KEY_SEPARATOR);
//   } else {
//     // A filter has been modified
//     // For the moment, do nothing
//     nextKey = "";
//   }
//   return nextKey;
// }
export const union = arrays => {
  let res = arrays.reduce((acc, array) => {
    if (!acc.length) {
      acc = array || [];
    } else {
      acc = acc.concat(array);
    }
    return acc;
  }, []);
  res = [...new Set(res)];
  // res.sort((a,b)=>);
  return res;
};

// export const except = arrays => {
//   let res = arrays.reduce((acc, array) => {
//     if (!acc.length) {
//       acc = array || [];
//     } else {
//       acc = acc.concat(array);
//     }
//     return acc;
//   }, []);
//   res = [...new Set(res)];
//   // res.sort((a,b)=>);
//   return res;
// };

export const inter = (arg0, arg1) => {
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
};
export const intersec = arrays => {
  if (!arrays.length) {
    return [];
  } else if (arrays.length === 1) {
    return arrays[0];
  } else {
    return arrays.slice(1).reduce((acc, array) => inter(acc, array), arrays[0]);
  }
};
export const hasInter = (arg0, arg1) => {
  if (isUndefined(arg0) || isUndefined(arg1)) {
    return true;
  } else {
    const n = arg0.length;
    const m = arg1.length;
    let i = 0;
    let j = 0;
    // const res = [];
    while (i < n && j < m) {
      if (arg0[i] > arg1[j]) {
        j += 1;
      } else if (arg0[i] < arg1[j]) {
        i += 1;
      } else {
        return true;
      }
    }
    return false;
  }
};
