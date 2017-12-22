/**
 * Returns whether or not obj is a number
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isNumber(obj) {
  return Object.prototype.toString.apply(obj) === "[object Number]";
}
/**
 * Returns whether or not obj is a Date object.
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isDate(obj) {
  return Object.prototype.toString.apply(obj) === "[object Date]";
}
/**
 * Returns whether or not obj is a string
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isString(obj) {
  return typeof obj === "string";
}

export function isStringOrNumber(obj) {
  const type = typeof obj;
  return type === "string" || type === "number";
}
/**
 * Returns whether or not obj is a regular expression object
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isRegExp(obj) {
  return Object.prototype.toString.apply(obj) === "[object RegExp]";
}
/**
 * Returns whether or not obj is a function object
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isFunction(obj) {
  return Object.prototype.toString.apply(obj) === "[object Function]";
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
  return re.replace(/[-/\\^$*+?.()|[\]{}]/g, "\\$&");
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

export function isInRange(
  { columns: columnIndex, rows: rowIndex },
  { columns: columnIndexStart, rows: rowIndexStart },
  { columns: columnIndexEnd, rows: rowIndexEnd }
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
  if (typeof accessor === "string") {
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
// date functions
// export const isDate = d => d instanceof Date && !isNaN(d.valueOf());
export const dateToString = (d, fmt) => {
  // format not managed yet
  if (!isDate(d)) {
    return undefined;
  }
  return `${d.getDate()}/${d.getMonth() + 1}/${d.getFullYear()}`;
};

const daysByMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
export const stringToDate = (s, fmt) => {
  // format not managed yet
  //  a voir culture
  if (!s) {
    return null;
  }
  const ds = s.split(/[.:/ ]+/g);
  const d = ds.map(v => parseInt(v, 8));
  if (d.length !== 3) {
    return undefined;
  }
  // a voir 12 mois nb days...
  // months 0..11 ??????
  if (ds[1] > 12) {
    return undefined;
  }
  if (ds[2] < 2000 || ds[2] > 3000) {
    return undefined;
  }
  const nDays = daysByMonth[d[1] - 1] + (d[1] === 2 && !(d[2] % 4));
  if (d[0] > nDays) {
    return undefined;
  }
  return !isNaN(Date.parse(`${ds[2]}-${ds[1]}-${ds[0]}`))
    ? new Date(`${ds[2]}-${ds[1].padStart(2, 0)}-${ds[0].padStart(2, 0)}`)
    : undefined;
};
export const isNavigationKey = e =>
  (e.which > 32 && e.which < 41) || // arrows...
  e.which === 9 || // tab
  e.which === 27 || // escape
  (e.which === 65 && (e.metaKey || e.ctrlKey));
