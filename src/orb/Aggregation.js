export function toAggregateFunc(func) {
  if (func) {
    if (typeof func === 'string' && eval(func)) { // eslint-disable-line no-eval
      return eval(func);  // eslint-disable-line no-eval
    } else if (typeof func === 'function') {
      return func;
    }
  }
  return sum;
}

export function count(datafield, intersection, data) {
  return intersection === 'all' ? data.length : intersection.length;
}
export function sum(datafield, intersection, data) {
  let sum = 0;
  forEachIntersection(datafield, intersection, data, (val) => {
    sum += val;
  });
  return sum;
}
export function min(datafield, intersection, data) {
  let min = null;
  forEachIntersection(datafield, intersection, data, (val) => {
    if (min == null || val < min) {
      min = val;
    }
  });
  return min;
}
export function max(datafield, intersection, data) {
  let max = null;
  forEachIntersection(datafield, intersection, data, (val) => {
    if (max == null || val > max) {
      max = val;
    }
  });
  return max;
}
export function avg(datafield, intersection, data) {
  let avg = 0;
  const len = (intersection === 'all' ? data : intersection).length;
  if (len > 0) {
    forEachIntersection(datafield, intersection, data, (val) => {
      avg += val;
    });
    avg /= len;
  }
  return avg;
}
export function prod(datafield, intersection, data) {
  let prod;
  const len = (intersection === 'all' ? data : intersection).length;
  if (len > 0) {
    prod = 1;
    forEachIntersection(datafield, intersection, data, (val) => {
      prod *= val;
    });
  }
  return prod;
}
export function stdev(datafield, intersection, data) {
  return Math.sqrt(calcVariance(datafield, intersection, data, false));
}
export function stdevp(datafield, intersection, data) {
  return Math.sqrt(calcVariance(datafield, intersection, data, true));
}
export function _var(datafield, intersection, data) {
  return calcVariance(datafield, intersection, data, false);
}
export function varp(datafield, intersection, data) {
  return calcVariance(datafield, intersection, data, true);
}
// }

export function calcVariance(datafield, intersection, data, population) {
  let variance = 0;
  let avg = 0;
  const len = (intersection === 'all' ? data : intersection).length;
  if (len > 0) {
    if (population || len > 1) {
      forEachIntersection(datafield, intersection, data, (val) => {
        avg += val;
      });
      avg /= len;
      forEachIntersection(datafield, intersection, data, (val) => {
        variance += (val - avg) * (val - avg);
      });
      variance /= (population ? len : len - 1);
    } else {
      variance = NaN;
    }
  }
  return variance;
}

export function forEachIntersection(datafield, intersection, data, callback) {
  // const all = intersection === 'all';
  // intersection = all ? data : intersection;
  if (intersection.length > 0) {
    for (let i = 0; i < intersection.length; i += 1) {
      // callback((all ? intersection[i] : data[intersection[i]])[datafield]);
      callback(data[intersection[i]][datafield]);
    }
  }
}
