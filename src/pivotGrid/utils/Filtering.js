export const ALL = "#All#";
export const NONE = "#None#";
export const BLANK = '#Blank#"';

export default function pass(filter, value) {
  if (Array.isArray(filter)) {
    return filter.includes(value);
    //   return (filter.excludeStatic && !found) || (!filter.excludeStatic && found);
    // } else if (filter.term) {
    //   return filter.operator.func(value, filter.term);
    // } else if (filter.staticValue === true || filter.staticValue === ALL) {
    //   return true;
    // } else if (filter.staticValue === false || filter.staticValue === NONE) {
    //   return false;
  }
  return true;
}
