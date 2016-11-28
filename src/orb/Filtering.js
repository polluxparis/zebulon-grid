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

export class ExpressionFilter {

  constructor(fieldName, operator, term, staticValue, excludeStatic) {
    this.fieldName = fieldName;
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

  pass(value) {
    if (Array.isArray(this.staticValue)) {
      const found = this.staticValue.indexOf(value) >= 0;
      return (this.excludeStatic && !found) || (!this.excludeStatic && found);
    } else if (this.term) {
      return this.operator.func(value, this.term);
    } else if (this.staticValue === true || this.staticValue === ALL) {
      return true;
    } else if (this.staticValue === false || this.staticValue === NONE) {
      return false;
    }
    return true;
  }

  isAlwaysTrue() {
    return !(
      this.term
      || Array.isArray(this.staticValue)
      || this.staticValue === NONE
      || this.staticValue === false
    );
  }
}
