'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ExpressionFilter = exports.Operators = exports.BLANK = exports.NONE = exports.ALL = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _Utils = require('./Utils');

var utils = _interopRequireWildcard(_Utils);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ALL = exports.ALL = '#All#';
var NONE = exports.NONE = '#None#';
var BLANK = exports.BLANK = '#Blank#"';

var Operators = exports.Operators = {
  get: function get(opname) {
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
    func: function func(value, term) {
      if (value) {
        return value.toString().search(utils.isRegExp(term) ? term : new RegExp(term, 'i')) >= 0;
      } else {
        return !term;
      }
    },
    regexpSupported: true
  },
  NOTMATCH: {
    name: 'Does Not Match',
    func: function func(value, term) {
      if (value) {
        return value.toString().search(utils.isRegExp(term) ? term : new RegExp(term, 'i')) < 0;
      } else {
        return !!term;
      }
    },
    regexpSupported: true
  },
  EQ: {
    name: '=',
    func: function func(value, term) {
      return value === term;
    },
    regexpSupported: false
  },
  NEQ: {
    name: '<>',
    func: function func(value, term) {
      return value !== term;
    },
    regexpSupported: false
  },
  GT: {
    name: '>',
    func: function func(value, term) {
      return value > term;
    },
    regexpSupported: false
  },
  GTE: {
    name: '>=',
    func: function func(value, term) {
      return value >= term;
    },
    regexpSupported: false
  },
  LT: {
    name: '<',
    func: function func(value, term) {
      return value < term;
    },
    regexpSupported: false
  },
  LTE: {
    name: '<=',
    func: function func(value, term) {
      return value <= term;
    },
    regexpSupported: false
  }
};

var ExpressionFilter = exports.ExpressionFilter = function () {
  function ExpressionFilter(fieldname, data, operator, term, staticValue, excludeStatic) {
    _classCallCheck(this, ExpressionFilter);

    this.fieldname = fieldname;
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
    this.data = data;
    this.filteredIds = this.getfilteredIds();
  }

  _createClass(ExpressionFilter, [{
    key: 'getfilteredIds',
    value: function getfilteredIds() {
      var res = [];
      for (var i = 0; i < this.data.length; i++) {
        var row = this.data[i];
        if (this.test(row[this.fieldname])) {
          res.push(i);
        }
      }
      return res;
    }
  }, {
    key: 'test',
    value: function test(value) {
      if (utils.isArray(this.staticValue)) {
        var found = this.staticValue.indexOf(value) >= 0;
        return this.excludeStatic && !found || !this.excludeStatic && found;
      } else if (this.term) {
        return this.operator.func(value, this.term);
      } else if (this.staticValue === true || this.staticValue === ALL) {
        return true;
      } else if (this.staticValue === false || this.staticValue === NONE) {
        return false;
      } else {
        return true;
      }
    }
  }, {
    key: 'isAlwaysTrue',
    value: function isAlwaysTrue() {
      return !(this.term || utils.isArray(this.staticValue) || this.staticValue === NONE || this.staticValue === false);
    }
  }]);

  return ExpressionFilter;
}();