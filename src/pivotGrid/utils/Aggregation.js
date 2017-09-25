function forEachIntersection(accessor, intersection, data, callback) {
  if (intersection && intersection.length > 0) {
    for (let i = 0; i < intersection.length; i += 1) {
      callback(accessor(data[intersection[i]]));
    }
  }
}

export function count(accessor, intersection) {
  return intersection.length;
}
export function sum(accessor, intersection, data) {
  let sum = null;
  forEachIntersection(accessor, intersection, data, val => {
    sum += val;
  });
  return sum;
}
export function min(accessor, intersection, data) {
  let min = null;
  forEachIntersection(accessor, intersection, data, val => {
    if (min == null || val < min) {
      min = val;
    }
  });
  return min;
}
export function max(accessor, intersection, data) {
  let max = null;
  forEachIntersection(accessor, intersection, data, val => {
    if (max == null || val > max) {
      max = val;
    }
  });
  return max;
}
export function avg(accessor, intersection, data) {
  let avg = null;
  const len = intersection.length;
  if (len > 0) {
    forEachIntersection(accessor, intersection, data, val => {
      avg += val;
    });
    avg /= len;
  }
  return avg;
}
export function weighted_avg(accessor, intersection, data) {
  let wavg = [0, 0];
  const len = intersection.length;
  if (len > 0) {
    forEachIntersection(accessor, intersection, data, val => {
      wavg[0] += val[0];
      wavg[1] += val[1];
    });
  }
  return wavg[0] / wavg[1];
}
export function delta(accessor, intersection, data) {
  let values = [0, 0];
  const len = intersection.length;
  if (len > 0) {
    forEachIntersection(accessor, intersection, data, val => {
      values[0] += val[0];
      values[1] += val[1];
    });
  }
  return values[0] - values[1];
}
export function prod(accessor, intersection, data) {
  let prod = null;
  const len = intersection.length;
  if (len > 0) {
    prod = 1;
    forEachIntersection(accessor, intersection, data, val => {
      prod *= val;
    });
  }
  return prod;
}

export function calcVariance(accessor, intersection, data, population) {
  let variance = 0;
  let avg = null;
  const len = intersection.length;
  if (len > 0) {
    if (population || len > 1) {
      forEachIntersection(accessor, intersection, data, val => {
        avg += val;
      });
      avg /= len;
      forEachIntersection(accessor, intersection, data, val => {
        variance += (val - avg) * (val - avg);
      });
      variance /= population ? len : len - 1;
    } else {
      variance = NaN;
    }
  }
  return variance;
}

export function stdev(accessor, intersection, data) {
  return Math.sqrt(calcVariance(accessor, intersection, data, false));
}
export function stdevp(accessor, intersection, data) {
  return Math.sqrt(calcVariance(accessor, intersection, data, true));
}

/* eslint-disable no-underscore-dangle */
export function _var(accessor, intersection, data) {
  return calcVariance(accessor, intersection, data, false);
}
/* eslint-enable */

export function varp(accessor, intersection, data) {
  return calcVariance(accessor, intersection, data, true);
}
