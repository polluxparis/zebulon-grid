import { ROOT_ID } from '../constants';
/**
 * Returns whether or not obj is a number
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isNumber(obj) {
  return Object.prototype.toString.apply(obj) === '[object Number]';
}
/**
 * Returns whether or not obj is a Date object.
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isDate(obj) {
  return Object.prototype.toString.apply(obj) === '[object Date]';
}
/**
 * Returns whether or not obj is a string
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isString(obj) {
  return typeof obj === 'string';
}

export function isStringOrNumber(obj) {
  const type = typeof obj;
  return type === 'string' || type === 'number';
}
/**
 * Returns whether or not obj is a regular expression object
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isRegExp(obj) {
  return Object.prototype.toString.apply(obj) === '[object RegExp]';
}
/**
 * Returns whether or not obj is a function object
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isFunction(obj) {
  return Object.prototype.toString.apply(obj) === '[object Function]';
}

/**
 * Returns whether or not obj is a Promise
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isPromise(obj) {
  return Promise.resolve(obj) === obj;
}
/**
 * Returns whether an object is empty
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isEmpty(obj) {
  return (
    isNullOrUndefined(obj) ||
    (obj.constructor === Object && Object.keys(obj).length === 0)
  );
}

/**
 * Returns whether or not obj is an observable
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isObservable(obj) {
  return obj && obj.subscribe !== undefined;
}

/**
 * Escapes all RegExp special characters.
 */
export function escapeRegex(re) {
  return re.replace(/[-/\\^$*+?.()|[\]{}]/g, '\\$&');
}
/**
 * Returns the first element in the array that satisfies the given predicate
 * @param  {Array} array     the array to search
 * @param  {function} predicate Function to apply to each element until it returns true
 * @return {Object}           The first object in the array that satisfies the predicate or undefined.
 */
export function findInArray(array, predicate) {
  if (this.isArray(array) && predicate) {
    for (let i = 0; i < array.length; i += 1) {
      const item = array[i];
      if (predicate(item)) {
        return item;
      }
    }
  }
  return undefined;
}

export function twoArraysIntersect(arg0, arg1) {
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

export function isInRange(
  { columnIndex, rowIndex },
  { columnIndex: columnIndexStart, rowIndex: rowIndexStart },
  { columnIndex: columnIndexEnd, rowIndex: rowIndexEnd }
) {
  let inRows = false;
  if (columnIndexStart <= columnIndexEnd) {
    inRows = columnIndexStart <= columnIndex && columnIndex <= columnIndexEnd;
  } else {
    inRows = columnIndexEnd <= columnIndex && columnIndex <= columnIndexStart;
  }
  let inColumns = false;
  if (rowIndexStart <= rowIndexEnd) {
    inColumns = rowIndexStart <= rowIndex && rowIndex <= rowIndexEnd;
  } else {
    inColumns = rowIndexEnd <= rowIndex && rowIndex <= rowIndexStart;
  }
  return inRows && inColumns;
}

export function isNull(obj) {
  return obj === null;
}

export function isUndefined(obj) {
  return obj === undefined;
}

export function isNullOrUndefined(obj) {
  return isUndefined(obj) || isNull(obj);
}

export function toAccessorFunction(accessor) {
  if (typeof accessor === 'string') {
    return row => row[accessor];
  }
  return accessor;
}

/* eslint-disable no-param-reassign */
export const range = (a, b) =>
  Array.from(
    (function*(x, y) {
      while (x <= y) {
        yield x;
        x += 1;
      }
    })(a, b)
  );
