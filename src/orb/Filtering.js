import * as utils from './utils/generic';

export const ALL = '#All#';
export const NONE = '#None#';
export const BLANK = '#Blank#"';

export const Operators = {
  get(opname) {
    switch (opname) {
      case this.MATCH.name:
        return this.MATCH;
      case this.NOTMATCH.name:
        return this.NOTMATCH;
      case this.EQ.name:
        return this.EQ;
      case this.NEQ.name:
        return this.NEQ;
      case this.GT.name:
        return this.GT;
      case this.GTE.name:
        return this.GTE;
      case this.LT.name:
        return this.LT;
      case this.LTE.name:
        return this.LTE;
      default:
        return this.NONE;
    }
  },
  NONE: null,
  MATCH: {
    name: 'Matches',
    func(value, term) {
      if (value) {
        return value.toString().search(utils.isRegExp(term) ? term : new RegExp(term, 'i')) >= 0;
      }
      return !(term);
    },
    regexpSupported: true,
  },
  NOTMATCH: {
    name: 'Does Not Match',
    func(value, term) {
      if (value) {
        return value.toString().search(utils.isRegExp(term) ? term : new RegExp(term, 'i')) < 0;
      }
      return !!term;
    },
    regexpSupported: true,
  },
  EQ: {
    name: '=',
    func(value, term) {
      return value === term;
    },
    regexpSupported: false,
  },
  NEQ: {
    name: '<>',
    func(value, term) {
      return value !== term;
    },
    regexpSupported: false,
  },
  GT: {
    name: '>',
    func(value, term) {
      return value > term;
    },
    regexpSupported: false,
  },
  GTE: {
    name: '>=',
    func(value, term) {
      return value >= term;
    },
    regexpSupported: false,
  },
  LT: {
    name: '<',
    func(value, term) {
      return value < term;
    },
    regexpSupported: false,
  },
  LTE: {
    name: '<=',
    func(value, term) {
      return value <= term;
    },
    regexpSupported: false,
  },
};

export function pass(filter, value) {
  if (Array.isArray(filter.staticValue)) {
    const found = filter.staticValue.includes(value);
    return (filter.excludeStatic && !found) || (!filter.excludeStatic && found);
  } else if (filter.term) {
    return filter.operator.func(value, filter.term);
  } else if (filter.staticValue === true || filter.staticValue === ALL) {
    return true;
  } else if (filter.staticValue === false || filter.staticValue === NONE) {
    return false;
  }
  return true;
}

function isAlwaysTrue() {
  return !(
    this.term
    || Array.isArray(this.staticValue)
    || this.staticValue === NONE
    || this.staticValue === false
  );
}


export class ExpressionFilter {

  constructor(fieldId, operator, term, staticValue, excludeStatic) {
    this.fieldId = fieldId;
    this.regexpMode = false;
    this.operator = Operators.get(operator);
    this.term = term || null;
    if (this.term && this.operator && this.operator.regexpSupported) {
      if (utils.isRegExp(this.term)) {
        this.regexpMode = true;
        if (!this.term.ignoreCase) {
          this.term = new RegExp(this.term.source, 'i');
        }
      }
    }

    this.staticValue = staticValue;
    this.excludeStatic = excludeStatic;
  }
}
