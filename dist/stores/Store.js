'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _rxLite = require('rx-lite');

var _Axe = require('../Axe');

var _AxeUi = require('../AxeUi');

var _AxeUi2 = _interopRequireDefault(_AxeUi);

var _Config = require('../Config');

var _Filtering = require('../Filtering');

var _Utils = require('../Utils');

var utils = _interopRequireWildcard(_Utils);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Creates a new instance of store
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
var Store = function () {
  function Store(config, forceUpdateCallback) {
    var _this = this;

    _classCallCheck(this, Store);

    this.init = false;
    this.dataMatrix = {};
    this.defaultfield = { name: '#undefined#' };
    this.forceUpdateCallback = forceUpdateCallback;
    this.config = new _Config.Config(config);
    this.filters = new Map();
    Object.keys(this.config.preFilters).forEach(function (key) {
      return _this.filters.set(key, _this.config.preFilters[key]);
    });
    this.sizes = this.getsizes();
    this.rowsUi = this.getrowsUi();
    this.columnsUi = this.getcolumnsUi();
    this.layout = this.getlayout();
    this.init = true;
  }

  _createClass(Store, [{
    key: 'subscribe',
    value: function subscribe(datasource) {
      this.data = [];
      this.filteredData = [];
      var observableDatasource = null;
      // datasource can be an observable, an array of arrays or an array of objects
      if (Array.isArray(datasource) && (Array.isArray(datasource[0]) || _typeof(datasource[0]) === 'object')) {
        observableDatasource = _rxLite.Observable.of(datasource);
      } else if (_rxLite.Observable.isObservable(datasource)) {
        // datasource is a Rxjs observable
        observableDatasource = datasource;
      }
      if (observableDatasource) this.dataSubscription = observableDatasource.subscribe(this.push.bind(this));
    }
  }, {
    key: 'unsubscribe',
    value: function unsubscribe() {
      if (this.dataSubscription) this.dataSubscription.dispose();
    }
  }, {
    key: 'push',
    value: function push(payload) {
      var _this2 = this;

      var pushed = void 0;
      var _data = this.data;
      // Push data (array of objects, array of arrays or object) to this.data
      if (Array.isArray(payload) && (Array.isArray(payload[0]) || _typeof(payload[0]) === 'object')) {
        payload.forEach(function (line) {
          _data.push(line);
        });
        pushed = payload;
      } else if (Array.isArray(payload) || (typeof payload === 'undefined' ? 'undefined' : _typeof(payload)) === 'object') {
        _data.push(payload);
        pushed = [payload];
      }
      // Push filtered data and refresh Ui
      if (pushed) {
        var filteredPush = this.filter(pushed);
        if (filteredPush.length) {
          filteredPush.forEach(function (line) {
            _this2.filteredData.push(line);
          });
          this.columnsUi = this.getcolumnsUi();
          this.rowsUi = this.getrowsUi();
          this.layout = this.getlayout();
          if (this.init) {
            this.forceUpdateCallback();
          }
        }
      }
      this.data = _data;
    }
  }, {
    key: 'filter',
    value: function filter(data) {
      var filterFields = [].concat(_toConsumableArray(this.filters.keys()));
      if (filterFields.length > 0) {
        var res = [];

        for (var i = 0; i < data.length; i++) {
          var row = data[i];
          var exclude = false;
          for (var fi = 0; fi < filterFields.length; fi++) {
            var fieldname = filterFields[fi];
            var fieldFilter = this.filters.get(fieldname);

            if (fieldFilter && !fieldFilter.test(row[fieldname])) {
              exclude = true;
              break;
            }
          }
          if (!exclude) {
            res.push(row);
          }
        }
        return res;
      } else {
        return data;
      }
    }
  }, {
    key: 'getrows',
    value: function getrows() {
      return new _Axe.Axe(_Axe.AxeType.ROWS, this.config.rowFields, this);
    }
  }, {
    key: 'getcolumns',
    value: function getcolumns() {
      return new _Axe.Axe(_Axe.AxeType.COLUMNS, this.config.columnFields, this);
    }
  }, {
    key: 'getChartAxe',
    value: function getChartAxe() {
      return new _Axe.Axe(_Axe.AxeType.CHART, [this.config.selectedField], this);
    }
  }, {
    key: 'getrowsUi',
    value: function getrowsUi(noNewAxe) {
      if (!noNewAxe) {
        this.rows = this.getrows();
      }
      return new _AxeUi2.default(this.rows);
    }
  }, {
    key: 'getcolumnsUi',
    value: function getcolumnsUi(noNewAxe) {
      if (!noNewAxe) {
        this.columns = this.getcolumns();
      }
      return new _AxeUi2.default(this.columns);
    }
  }, {
    key: 'getlayout',
    value: function getlayout() {
      var rowHeaders = {
        width: (this.rows.fields.length || 1) + (this.config.dataHeadersLocation === 'rows' && this.config.activatedDataFieldsCount > 1 ? 1 : 0),
        height: this.rowsUi.headers.length
      };
      var columnHeaders = {
        width: this.columnsUi.headers.length,
        height: (this.columns.fields.length || 1) + (this.config.dataHeadersLocation === 'columns' && this.config.activatedDataFieldsCount > 1 ? 1 : 0)
      };
      var pivotTable = {
        width: rowHeaders.width + columnHeaders.width,
        height: rowHeaders.height + columnHeaders.height
      };
      return { columnHeaders: columnHeaders, rowHeaders: rowHeaders, pivotTable: pivotTable };
    }
  }, {
    key: 'getsizes',
    value: function getsizes() {
      var cell = {
        height: this.config.cellHeight || 30,
        width: this.config.cellWidth || 100
      };
      var grid = {
        width: this.config.width,
        height: this.config.height
      };
      return { cell: cell, grid: grid };
    }
  }, {
    key: 'sort',
    value: function sort(axetype, field) {
      var sorted = false;
      if (axetype === _Axe.AxeType.ROWS) {
        this.rows.sort(field);
        sorted = true;
      } else if (axetype === _Axe.AxeType.COLUMNS) {
        this.columns.sort(field);
        sorted = true;
      }
      if (sorted && this.init) {
        switch (axetype) {
          case _Axe.AxeType.ROWS:
            this.rowsUi = this.getrowsUi(true);
            break;
          case _Axe.AxeType.COLUMNS:
            this.columnsUi = this.getcolumnsUi(true);
            break;
          default:
            break;
        }
        this.forceUpdateCallback();
      }
    }
  }, {
    key: 'moveField',
    value: function moveField(fieldname, oldaxetype, newaxetype, position) {
      var axeType = this.config.moveField(fieldname, oldaxetype, newaxetype, position);
      switch (axeType) {
        case _Axe.AxeType.COLUMNS:
          this.columnsUi = this.getcolumnsUi();
          break;
        case _Axe.AxeType.ROWS:
          this.rowsUi = this.getrowsUi();
          break;
        default:
          this.columnsUi = this.getcolumnsUi();
          this.rowsUi = this.getrowsUi();
      }
      this.layout = this.getlayout();
      this.forceUpdateCallback();
    }
  }, {
    key: 'selectField',
    value: function selectField(fieldname) {
      this.config.selectField(fieldname);
      this.rows = this.getrows();
      this.forceUpdateCallback();
    }
  }, {
    key: 'toggleDataField',
    value: function toggleDataField(fieldname) {
      // toggleDataField returns the count of activated data fields.
      // If it is 0, there is no need to recompute the axes as the only effect is to make the data cells blank.
      if (this.config.toggleDataField(fieldname)) {
        switch (this.config.dataHeadersLocation) {
          case 'columns':
            this.columnsUi = this.getcolumnsUi();
            break;
          case 'rows':
            this.rowsUi = this.getrowsUi();
            break;
          default:
            break;
        }
        this.layout = this.getlayout();
      }
      this.forceUpdateCallback();
    }
  }, {
    key: 'applyFilter',
    value: function applyFilter(fieldname, axetype, all, operator, term, staticValue, excludeStatic) {
      if (all && this.filters.has(fieldname)) {
        this.filters.delete(fieldname);
      } else if (!all) {
        this.filters.set(fieldname, new _Filtering.ExpressionFilter(fieldname, this.filteredData, operator, term, staticValue, excludeStatic));
      }
      this.filteredData = this.filter(this.data);
      this.columnsUi = this.getcolumnsUi();
      this.rowsUi = this.getrowsUi();
      this.layout = this.getlayout();
      this.forceUpdateCallback();
    }
  }, {
    key: 'drilldown',
    value: function drilldown(cell) {
      return this.config.drilldown(cell);
    }
  }, {
    key: 'refreshData',
    value: function refreshData(data) {
      this.filteredData = data;
    }
  }, {
    key: 'toggleSubtotals',
    value: function toggleSubtotals(axetype) {
      if (this.config.toggleSubtotals(axetype)) {}
    }
  }, {
    key: 'toggleGrandtotal',
    value: function toggleGrandtotal(axetype) {
      if (this.config.toggleGrandtotal(axetype)) {}
    }
  }, {
    key: 'areSubtotalsVisible',
    value: function areSubtotalsVisible(axetype) {
      return this.config.areSubtotalsVisible(axetype);
    }
  }, {
    key: 'isGrandtotalVisible',
    value: function isGrandtotalVisible(axetype) {
      return this.config.isGrandtotalVisible(axetype);
    }
  }, {
    key: 'getFieldValues',
    value: function getFieldValues(field, filterFunc) {
      var values1 = [];
      var values = [];
      var containsBlank = false;
      // We use config.data here instead of filteredData because otherwise you lose the filtered values the next time you open a Filter Panel
      for (var i = 0; i < this.data.length; i++) {
        var row = this.data[i];
        var val = row[field];
        if (filterFunc !== undefined) {
          if (filterFunc === true || typeof filterFunc === 'function' && filterFunc(val)) {
            values1.push(val);
          }
        } else {
          if (val != null) {
            values1.push(val);
          } else {
            containsBlank = true;
          }
        }
      }
      if (values1.length > 1) {
        if (utils.isNumber(values1[0]) || utils.isDate(values1[0])) {
          values1.sort(function (a, b) {
            return a ? b ? a - b : 1 : b ? -1 : 0;
          });
        } else {
          values1.sort();
        }

        for (var vi = 0; vi < values1.length; vi++) {
          if (vi === 0 || values1[vi] !== values[values.length - 1]) {
            values.push(values1[vi]);
          }
        }
      } else {
        values = values1;
      }
      if (containsBlank) {
        values.unshift(null);
      }
      return values;
    }
  }, {
    key: 'getFieldFilter',
    value: function getFieldFilter(field) {
      return this.filters[field];
    }
  }, {
    key: 'isFieldFiltered',
    value: function isFieldFiltered(field) {
      var filter = this.getFieldFilter(field);
      return filter != null && !filter.isAlwaysTrue();
    }
  }, {
    key: 'getData',
    value: function getData(field, rowdim, coldim, aggregateFunc) {
      var value = void 0;
      if (rowdim && coldim) {
        var datafieldName = field || (this.config.activatedDataFields[0] || this.defaultfield).name;
        value = this.calcAggregation(rowdim.isRoot ? null : rowdim.getRowIndexes().slice(0), coldim.isRoot ? null : coldim.getRowIndexes().slice(0), [datafieldName], aggregateFunc)[datafieldName];
      }
      return value === undefined ? null : value;
    }
  }, {
    key: 'calcAggregation',
    value: function calcAggregation(rowIndexes, colIndexes, fieldNames, aggregateFunc) {
      var res = {};

      if (this.config.activatedDataFieldsCount > 0) {
        var intersection = void 0;

        if (rowIndexes === null) {
          intersection = colIndexes;
        } else if (colIndexes === null) {
          intersection = rowIndexes;
        } else {
          intersection = utils.twoArraysIntersect(colIndexes, rowIndexes);
        }

        var emptyIntersection = intersection && intersection.length === 0;
        var data = this.filteredData;
        var datafield = void 0;
        var datafields = [];

        if (fieldNames) {
          for (var fieldnameIndex = 0; fieldnameIndex < fieldNames.length; fieldnameIndex++) {
            datafield = this.config.getDataField(fieldNames[fieldnameIndex]);
            if (!aggregateFunc) {
              if (!datafield) {
                datafield = this.config.getField(fieldNames[fieldnameIndex]);
                if (datafield) {
                  aggregateFunc = datafield.dataSettings ? datafield.dataSettings.aggregateFunc() : datafield.aggregateFunc();
                }
              } else {
                aggregateFunc = datafield.aggregateFunc();
              }
            }

            if (datafield && aggregateFunc) {
              datafields.push({ field: datafield, aggregateFunc: aggregateFunc });
            }
          }
        } else {
          for (var datafieldIndex = 0; datafieldIndex < this.config.activatedDataFieldsCount; datafieldIndex++) {
            datafield = this.config.activatedDataFields[datafieldIndex] || this.defaultfield;
            if (aggregateFunc || datafield.aggregateFunc) {
              datafields.push({ field: datafield, aggregateFunc: aggregateFunc || datafield.aggregateFunc() });
            }
          }
        }

        for (var dfi = 0; dfi < datafields.length; dfi++) {
          datafield = datafields[dfi];
          // no data
          if (emptyIntersection) {
            res[datafield.field.name] = null;
          } else {
            res[datafield.field.name] = datafield.aggregateFunc(datafield.field.name, intersection || 'all', data, rowIndexes, colIndexes);
          }
        }
      }

      return res;
    }
  }]);

  return Store;
}();

exports.default = Store;