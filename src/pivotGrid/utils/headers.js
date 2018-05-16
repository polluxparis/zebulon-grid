import { KEY_SEPARATOR, ROOT_ID, TOTAL_ID } from "../constants";
import { utils } from "zebulon-controls";

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
    ((node.id !== ROOT_ID ||
      (node.dimensionId === TOTAL_ID && node.isVisible)) &&
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
  // node.parent = null;
  node.options = null;
  node.sizes = null;
  node = undefined;
  return null;
};

export const resetDimensions = dimensions =>
  Object.values(dimensions).forEach(dimension => {
    Object.values(dimension.values).forEach(value => {
      value.rowIndexes = null;
      value = null;
    });
    dimension = null;
  });

// calculate the union of two arrays (arg1 is the second array) or an array of arrays (arg0)
// export const union = (arg0, arg1) => {
//   if (arg1) {
//     return [...new Set(arg[0].concat)];
//   }
//   let res;
//   if (arg1) {
//     res = [...arg[0].concat(arg[1])];
//   } else {
//     res = arg0.reduce((acc, array) => {
//       if (!acc.length) {
//         acc = array || [];
//       } else {
//         acc = acc.concat(array);
//       }
//       return acc;
//     }, []);
//   }
//   res = [...new Set(res)];
//   // res.sort((a,b)=>);
//   return res;
// };
// // calculate the intersection of two arrays (arg1 is the second array) or an array of arrays (arg0)
// export const intersection = (arg0, arg1) => {
//   const intersection_ = (arg0, arg1) => {
//     if (utils.isUndefined(arg0)) {
//       return arg1;
//     } else if (utils.isUndefined(arg1)) {
//       return arg0;
//     } else {
//       const n = arg0.length;
//       const m = arg1.length;
//       let i = 0;
//       let j = 0;
//       const res = [];
//       while (i < n && j < m) {
//         if (arg0[i] > arg1[j]) {
//           j += 1;
//         } else if (arg0[i] < arg1[j]) {
//           i += 1;
//         } else {
//           res.push(arg0[i]);
//           i += 1;
//           j += 1;
//         }
//       }
//       return res;
//     }
//   };
//   if (arg1) {
//     return intersection_(arg0, arg1);
//   } else if (!arg0.length) {
//     return [];
//   } else if (arg0.length === 1) {
//     return arg0[0];
//   } else {
//     return arg0
//       .slice(1)
//       .reduce((acc, array) => intersection_(acc, array), arg0[0]);
//   }
// };
// export const hasIntersection = (arg0, arg1) => {
//   if (utils.isUndefined(arg0) || utils.isUndefined(arg1)) {
//     return true;
//   } else {
//     const n = arg0.length;
//     const m = arg1.length;
//     let i = 0;
//     let j = 0;
//     // const res = [];
//     while (i < n && j < m) {
//       if (arg0[i] > arg1[j]) {
//         j += 1;
//       } else if (arg0[i] < arg1[j]) {
//         i += 1;
//       } else {
//         return true;
//       }
//     }
//     return false;
//   }
// };
