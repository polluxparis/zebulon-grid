// function forEachIntersection(accessor, intersection, data, callback) {
//   if (intersection && intersection.length > 0) {
//     // for (let i = 0; i < intersection.length; i += 1) {
//     //   const row=data[intersection[i]];
//     //   callback(accessor(row));
//     // }
//     intersection.map(index=>{const row =data[index];
//       if(!row.isFiltered){callback(accessor(row))};
//   }
// }

export const count = values => values.length;
export const sum = values =>
  values.reduce((sum, value) => (sum += value), null);
export const min = values =>
  values.reduce(
    (min, value) => (min = min === null || value < min ? value : min),
    null
  );
export const max = values =>
  values.reduce(
    (max, value) => (max = max === null || value > max ? value : max),
    null
  );
export const avg = values =>
  values.length === 0 ? null : sum(values) / values.length;
export const weighted_avg = values => {
  const wavg = values.reduce(
    (wavg, value) => {
      wavg.v0 += value.v0;
      wavg.v1 += value.v1;
      return wavg;
    },
    { v0: null, v1: null }
  );
  return wavg.v0 === null && wavg.v1 === null ? null : wavg.v0 / wavg.v1;
};
export const delta = values => {
  const delta = values.reduce(
    (delta, value) => {
      delta.v0 += value.v0;
      delta.v1 += value.v1;
      return delta;
    },
    { v0: null, v1: null }
  );
  return delta.v0 - delta.v1;
};
export const prod = values =>
  values.reduce((prod, value) => (prod *= value), null);

// export function calcVariance(accessor, intersection, data, population) {
//   let variance = 0;
//   let avg = null;
//   const len = intersection.length;
//   if (len > 0) {
//     if (population || len > 1) {
//       forEachIntersection(accessor, intersection, data, val => {
//         avg += val;
//       });
//       avg /= len;
//       forEachIntersection(accessor, intersection, data, val => {
//         variance += (val - avg) * (val - avg);
//       });
//       variance /= population ? len : len - 1;
//     } else {
//       variance = NaN;
//     }
//   }
//   return variance;
// }

// export function stdev(accessor, intersection, data) {
//   return Math.sqrt(calcVariance(accessor, intersection, data, false));
// }
// export function stdevp(accessor, intersection, data) {
//   return Math.sqrt(calcVariance(accessor, intersection, data, true));
// }

// /* eslint-disable no-underscore-dangle */
// export function _var(accessor, intersection, data) {
//   return calcVariance(accessor, intersection, data, false);
// }
// /* eslint-enable */

// export function varp(accessor, intersection, data) {
//   return calcVariance(accessor, intersection, data, true);
// }
