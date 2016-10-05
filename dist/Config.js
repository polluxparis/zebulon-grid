'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Config = exports.Field = exports.SortConfig = exports.SubTotalConfig = exports.GrandTotalConfig = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _Utils = require('./Utils');

var utils = _interopRequireWildcard(_Utils);

var _Axe = require('./Axe');

var _Aggregation = require('./Aggregation');

var aggregation = _interopRequireWildcard(_Aggregation);

var _Filtering = require('./Filtering');

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var GrandTotalConfig = exports.GrandTotalConfig = function GrandTotalConfig(options) {
  _classCallCheck(this, GrandTotalConfig);

  options = options || {};

  this.rowsvisible = options.rowsvisible !== undefined ? options.rowsvisible : true;
  this.columnsvisible = options.columnsvisible !== undefined ? options.columnsvisible : true;
};

var SubTotalConfig = exports.SubTotalConfig = function SubTotalConfig(options, setdefaults) {
  _classCallCheck(this, SubTotalConfig);

  var defaults = {
    visible: setdefaults === true ? true : undefined,
    collapsible: setdefaults === true ? true : undefined,
    collapsed: setdefaults === true ? false : undefined
  };
  options = options || {};

  this.visible = options.visible !== undefined ? options.visible : defaults.visible;
  this.collapsible = options.collapsible !== undefined ? options.collapsible : defaults.collapsible;
  this.collapsed = options.collapsed !== undefined ? options.collapsed : defaults.collapsed;
};

var SortConfig = exports.SortConfig = function SortConfig(options) {
  _classCallCheck(this, SortConfig);

  options = options || {};

  this.order = options.order || (options.customfunc ? 'asc' : null);
  this.customfunc = options.customfunc;
};

var Field = exports.Field = function () {
  function Field(options, createSubOptions) {
    _classCallCheck(this, Field);

    options = options || {};

    // field name
    this.name = options.name;
    this.code = options.code || this.name;

    // shared settings
    this.caption = options.caption || this.name;

    // rows & columns settings
    this.sort = new SortConfig(options.sort);
    this.subTotal = new SubTotalConfig(options.subTotal);

    this.aggregateFuncName = options.aggregateFuncName || (options.aggregateFunc ? utils.isString(options.aggregateFunc) ? options.aggregateFunc : 'custom' : null);

    this.aggregateFunc(options.aggregateFunc);
    this.formatFunc(options.formatFunc || this.defaultFormatFunc);

    if (createSubOptions !== false) {
      (this.rowSettings = new Field(options.rowSettings, false)).name = this.name;(this.columnSettings = new Field(options.columnSettings, false)).name = this.name;(this.dataSettings = new Field(options.dataSettings, false)).name = this.name;
    }
  }

  _createClass(Field, [{
    key: 'defaultFormatFunc',
    value: function defaultFormatFunc(val) {
      return val != null ? val.toString() : '';
    }
  }, {
    key: 'aggregateFunc',
    value: function aggregateFunc(func) {
      if (func) {
        this._aggregatefunc = aggregation.toAggregateFunc(func);
      } else {
        return this._aggregatefunc;
      }
    }
  }, {
    key: 'formatFunc',
    value: function formatFunc(func) {
      if (func) {
        this._formatfunc = func;
      } else {
        return this._formatfunc;
      }
    }
  }]);

  return Field;
}();

var getpropertyvalue = function getpropertyvalue(property, configs, defaultvalue) {
  for (var i = 0; i < configs.length; i++) {
    if (configs[i][property] != null) {
      return configs[i][property];
    }
  }
  return defaultvalue;
};

var mergefieldconfigs = function mergefieldconfigs() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  var merged = {
    configs: [],
    sorts: [],
    subtotals: [],
    functions: []
  };

  for (var i = 0; i < args.length; i++) {
    var nnconfig = args[i] || {};
    merged.configs.push(nnconfig);
    merged.sorts.push(nnconfig.sort || {});
    merged.subtotals.push(nnconfig.subTotal || {});
    merged.functions.push({
      aggregateFuncName: nnconfig.aggregateFuncName,
      aggregateFunc: i === 0 ? nnconfig.aggregateFunc : nnconfig.aggregateFunc ? nnconfig.aggregateFunc() : null,
      formatFunc: i === 0 ? nnconfig.formatFunc : nnconfig.formatFunc ? nnconfig.formatFunc() : null
    });
  }

  return merged;
};

var createfield = function createfield(rootconfig, axetype, fieldconfig, defaultfieldconfig) {
  var axeconfig;
  var fieldAxeconfig;

  if (defaultfieldconfig) {
    switch (axetype) {
      case _Axe.AxeType.ROWS:
        axeconfig = rootconfig.rowSettings;
        fieldAxeconfig = defaultfieldconfig.rowSettings;
        break;
      case _Axe.AxeType.COLUMNS:
        axeconfig = rootconfig.columnSettings;
        fieldAxeconfig = defaultfieldconfig.columnSettings;
        break;
      case _Axe.AxeType.DATA:
        axeconfig = rootconfig.dataSettings;
        fieldAxeconfig = defaultfieldconfig.dataSettings;
        break;
      default:
        axeconfig = null;
        fieldAxeconfig = null;
        break;
    }
  } else {
    axeconfig = null;
    fieldAxeconfig = null;
  }

  var merged = mergefieldconfigs(fieldconfig, fieldAxeconfig, axeconfig, defaultfieldconfig, rootconfig);

  return new Field({
    name: getpropertyvalue('name', merged.configs, ''),

    caption: getpropertyvalue('caption', merged.configs, ''),

    code: getpropertyvalue('code', merged.configs, ''),

    sort: {
      order: getpropertyvalue('order', merged.sorts, null),
      customfunc: getpropertyvalue('customfunc', merged.sorts, null)
    },
    subTotal: {
      visible: getpropertyvalue('visible', merged.subtotals, true),
      collapsible: getpropertyvalue('collapsible', merged.subtotals, true),
      collapsed: getpropertyvalue('collapsed', merged.subtotals, false) && getpropertyvalue('collapsible', merged.subtotals, true)
    },

    aggregateFuncName: getpropertyvalue('aggregateFuncName', merged.functions, 'sum'),
    aggregateFunc: getpropertyvalue('aggregateFunc', merged.functions, aggregation.sum),
    formatFunc: getpropertyvalue('formatFunc', merged.functions, null)
  }, false);
};

/**
 * Creates a new instance of pgrid config
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
// module.config(config) {

var Config = exports.Config = function () {
  function Config(config) {
    var _this = this;

    _classCallCheck(this, Config);

    this.config = config;
    this.canMoveFields = config.canMoveFields !== undefined ? !!config.canMoveFields : true;
    this.dataHeadersLocation = config.dataHeadersLocation === 'columns' ? 'columns' : 'rows';
    this.grandTotal = new GrandTotalConfig(config.grandTotal);
    this.subTotal = new SubTotalConfig(config.subTotal, true);
    this.width = config.width;
    this.height = config.height;
    this.cellWidth = config.cellWidth;
    this.cellHeight = config.cellHeight;

    this.rowSettings = new Field(config.rowSettings, false);
    this.columnSettings = new Field(config.columnSettings, false);
    this.dataSettings = new Field(config.dataSettings, false);

    this.allFields = (config.fields || []).map(function (fieldConfig) {
      return new Field(fieldConfig);
    });
    this.dataFields = (config.dataFields || []).map(function (fieldConfig) {
      return new Field(fieldConfig);
    });

    // map fields names to captions
    this.dataFieldNames = this.allFields.map(function (f) {
      return f.name;
    });
    this.dataFieldCaptions = this.allFields.map(function (f) {
      return f.caption;
    });

    this.rowFields = (config.rows || []).map(function (fieldconfig) {
      fieldconfig = _this.ensureFieldConfig(fieldconfig);
      return createfield(_this, _Axe.AxeType.ROWS, fieldconfig, _this.getfield(_this.allFields, fieldconfig.name));
    });

    this.columnFields = (config.columns || []).map(function (fieldconfig) {
      fieldconfig = _this.ensureFieldConfig(fieldconfig);
      return createfield(_this, _Axe.AxeType.COLUMNS, fieldconfig, _this.getfield(_this.allFields, fieldconfig.name));
    });

    this.selectedField = this.allFields.filter(function (field) {
      return field.caption === config.rows[0];
    })[0];

    this.activatedDataFields = this.dataFields.filter(function (field) {
      return config.data.indexOf(field.caption) > -1;
    });

    this.drilldown = config.drilldown || function (cell) {
      return console.log('drilldown on cell (default)', cell);
    };

    this.runtimeVisibility = {
      subtotals: {
        rows: this.rowSettings.subTotal.visible !== undefined ? this.rowSettings.subTotal.visible : true,
        columns: this.columnSettings.subTotal.visible !== undefined ? this.columnSettings.subTotal.visible : true
      }
    };
    this.activatedDataFieldsCount = this.getactivatedDataFieldsCount();
    this.availableFields = this.getavailableFields();
    this.preFilters = this.getpreFilters();
  }

  _createClass(Config, [{
    key: 'getactivatedDataFieldsCount',
    value: function getactivatedDataFieldsCount() {
      return this.activatedDataFields ? this.activatedDataFields.length : 0;
    }
  }, {
    key: 'getavailableFields',
    value: function getavailableFields() {
      var usedFields = [].concat(this.rowFields, this.columnFields);
      return this.allFields.filter(function (field) {
        return usedFields.map(function (field) {
          return field.name;
        }).indexOf(field.name) === -1;
      });
    }
  }, {
    key: 'getpreFilters',
    value: function getpreFilters() {
      var _this2 = this;

      var prefilters = {};
      if (this.config.preFilters) {
        utils.forEach(utils.ownProperties(this.config.preFilters), function (filteredField) {
          var filterName = _this2.captionToName(filteredField);
          var prefilterConfig = _this2.config.preFilters[filteredField];
          if (utils.isArray(prefilterConfig)) {
            prefilters[filterName] = new _Filtering.ExpressionFilter(filterName, _this2.data, null, null, prefilterConfig, false);
          } else {
            var opname = utils.ownProperties(prefilterConfig)[0];
            if (opname) {
              prefilters[filterName] = new _Filtering.ExpressionFilter(filterName, _this2.data, opname, prefilterConfig[opname]);
            }
          }
        });
      }
      return prefilters;
    }
  }, {
    key: 'captionToName',
    value: function captionToName(caption) {
      var fcaptionIndex = this.dataFieldCaptions.indexOf(caption);
      return fcaptionIndex >= 0 ? this.dataFieldNames[fcaptionIndex] : caption;
    }
  }, {
    key: 'nameToCaption',
    value: function nameToCaption(name) {
      var fnameIndex = this.dataFieldNames.indexOf(name);
      return fnameIndex >= 0 ? this.dataFieldCaptions[fnameIndex] : name;
    }
    // setTheme (newTheme) {
    //   return this.theme.current() !== this.theme.current(newTheme)
    // }

  }, {
    key: 'ensureFieldConfig',
    value: function ensureFieldConfig(obj) {
      if (typeof obj === 'string') {
        return {
          name: this.captionToName(obj)
        };
      }
      return obj;
    }
  }, {
    key: 'getfield',
    value: function getfield(axefields, fieldname) {
      var fieldindex = this.getfieldindex(axefields, fieldname);
      if (fieldindex > -1) {
        return axefields[fieldindex];
      }
      return null;
    }
  }, {
    key: 'getfieldindex',
    value: function getfieldindex(axefields, fieldname) {
      for (var fi = 0; fi < axefields.length; fi++) {
        if (axefields[fi].name === fieldname) {
          return fi;
        }
      }
      return -1;
    }
  }, {
    key: 'getField',
    value: function getField(fieldname) {
      return this.getfield(this.allFields, fieldname);
    }
  }, {
    key: 'getRowField',
    value: function getRowField(fieldname) {
      return this.getfield(this.rowFields, fieldname);
    }
  }, {
    key: 'getColumnField',
    value: function getColumnField(fieldname) {
      return this.getfield(this.columnFields, fieldname);
    }
  }, {
    key: 'getDataField',
    value: function getDataField(fieldname) {
      return this.getfield(this.dataFields, fieldname);
    }
  }, {
    key: 'moveField',
    value: function moveField(fieldname, oldaxetype, newaxetype, position) {
      var oldaxe, oldposition;
      var newaxe;
      var fieldConfig;
      var defaultFieldConfig = this.getfield(this.allFields, fieldname);

      if (defaultFieldConfig) {
        switch (oldaxetype) {
          case _Axe.AxeType.ROWS:
            oldaxe = this.rowFields;
            break;
          case _Axe.AxeType.COLUMNS:
            oldaxe = this.columnFields;
            break;
          case _Axe.AxeType.FIELDS:
            oldaxe = this.availableFields;
            break;
          default:
            break;
        }

        switch (newaxetype) {
          case _Axe.AxeType.ROWS:
            newaxe = this.rowFields;
            fieldConfig = this.getRowField(fieldname);
            break;
          case _Axe.AxeType.COLUMNS:
            newaxe = this.columnFields;
            fieldConfig = this.getColumnField(fieldname);
            break;
          case _Axe.AxeType.FIELDS:
            newaxe = this.availableFields;
            fieldConfig = this.getField(fieldname);
            break;
          default:
            break;
        }

        if (oldaxe || newaxe) {
          var newAxeSubtotalsState = this.areSubtotalsVisible(newaxetype);

          if (oldaxe) {
            oldposition = this.getfieldindex(oldaxe, fieldname);
            if (oldaxetype === newaxetype) {
              if (oldposition === (oldaxe.length - 1 && position == null) || oldposition === position - 1) {
                return false;
              }
            }
            oldaxe.splice(oldposition, 1);
          }

          var field = createfield(this, newaxetype, fieldConfig, defaultFieldConfig);

          if (!newAxeSubtotalsState && field.subTotal.visible !== false) {
            field.subTotal.visible = null;
          }

          if (newaxe) {
            if (position != null) {
              newaxe.splice(position, 0, field);
            } else {
              newaxe.push(field);
            }
          }
          if (newaxetype === _Axe.AxeType.FIELDS) {
            return oldaxetype;
          } else if (oldaxetype === _Axe.AxeType.FIELDS) {
            return newaxetype;
          } else {
            return -1;
          }
        }
      }
    }
  }, {
    key: 'selectField',
    value: function selectField(fieldname) {
      this.selectedField = this.getfield(this.allFields, fieldname);
    }
  }, {
    key: 'toggleDataField',
    value: function toggleDataField(fieldname) {
      var defaultFieldConfig = this.getDataField(fieldname);
      var newDataFields = this.activatedDataFields.filter(function (fld) {
        return fld.name !== fieldname;
      });
      if (this.activatedDataFields.length === newDataFields.length) {
        this.activatedDataFields.push(defaultFieldConfig);
      } else {
        this.activatedDataFields = newDataFields;
      }
      this.activatedDataFieldsCount = this.getactivatedDataFieldsCount();
      return this.activatedDataFieldsCount;
    }
  }, {
    key: 'toggleSubtotals',
    value: function toggleSubtotals(axetype) {
      var i;
      var axeFields;
      var newState = !this.areSubtotalsVisible(axetype);

      if (axetype === _Axe.AxeType.ROWS) {
        this.runtimeVisibility['subtotals']['rows'] = newState;
        axeFields = this.rowFields;
      } else if (axetype === _Axe.AxeType.COLUMNS) {
        this.runtimeVisibility['subtotals']['columns'] = newState;
        axeFields = this.columnFields;
      } else {
        return false;
      }

      newState = newState === false ? null : true;
      for (i = 0; i < axeFields.length; i++) {
        if (axeFields[i].subTotal.visible !== false) {
          axeFields[i].subTotal.visible = newState;
        }
      }
      return true;
    }
  }, {
    key: 'areSubtotalsVisible',
    value: function areSubtotalsVisible(axetype) {
      if (axetype === _Axe.AxeType.ROWS) {
        return this.runtimeVisibility['subtotals']['rows'];
      } else if (axetype === _Axe.AxeType.COLUMNS) {
        return this.runtimeVisibility['subtotals']['columns'];
      } else {
        return null;
      }
    }
  }, {
    key: 'toggleGrandtotal',
    value: function toggleGrandtotal(axetype) {
      var newState = !this.isGrandtotalVisible(axetype);

      if (axetype === _Axe.AxeType.ROWS) {
        this.grandTotal.rowsvisible = newState;
      } else if (axetype === _Axe.AxeType.COLUMNS) {
        this.grandTotal.columnsvisible = newState;
      } else {
        return false;
      }
      return true;
    }
  }, {
    key: 'isGrandtotalVisible',
    value: function isGrandtotalVisible(axetype) {
      if (axetype === _Axe.AxeType.ROWS) {
        return this.grandTotal.rowsvisible;
      } else if (axetype === _Axe.AxeType.COLUMNS) {
        return this.grandTotal.columnsvisible;
      } else {
        return false;
      }
    }
  }]);

  return Config;
}();