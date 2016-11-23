import * as utils from './utils/generic';
import { AxisType } from './Axis';
import * as aggregation from './Aggregation';
import { ExpressionFilter } from './Filtering';

export class GrandTotalConfig {

  constructor(options) {
    options = options || {};

    this.rowsvisible = options.rowsvisible !== undefined ? options.rowsvisible : true;
    this.columnsvisible = options.columnsvisible !== undefined ? options.columnsvisible : true;
  }
}

export class SubTotalConfig {

  constructor(options, setdefaults) {
    const defaults = {
      visible: setdefaults === true ? true : undefined,
      collapsible: setdefaults === true ? true : undefined,
      collapsed: setdefaults === true ? false : undefined,
    };
    options = options || {};

    this.visible = options.visible !== undefined ? options.visible : defaults.visible;
    this.collapsible = options.collapsible !== undefined ? options.collapsible : defaults.collapsible;
    this.collapsed = options.collapsed !== undefined ? options.collapsed : defaults.collapsed;
  }
}

export class SortConfig {

  constructor(options) {
    options = options || {};

    this.order = options.order || (options.customfunc ? 'asc' : null);
    this.customfunc = options.customfunc;
  }
}

export class Field {

  constructor(options, createSubOptions) {
    options = options || {};

    // field name
    this.name = options.name;
    this.code = options.code || this.name;

    // shared settings
    this.caption = options.caption || this.name;

    // rows & columns settings
    this.sort = new SortConfig(options.sort);
    this.subTotal = new SubTotalConfig(options.subTotal);

    this.aggregateFuncName = options.aggregateFuncName ||
    (options.aggregateFunc
      ? (utils.isString(options.aggregateFunc)
        ? options.aggregateFunc : 'custom')
        : null);

    this.aggregateFunc(options.aggregateFunc);
    this.formatFunc(options.formatFunc || this.defaultFormatFunc);

    if (createSubOptions !== false) {
      (this.rowSettings = new Field(options.rowSettings, false)).name = this.name
      ;(this.columnSettings = new Field(options.columnSettings, false)).name = this.name
      ;(this.dataSettings = new Field(options.dataSettings, false)).name = this.name;
    }
  }

  defaultFormatFunc(val) {
    return val != null ? val.toString() : '';
  }

  aggregateFunc(func) {
    if (func) {
      this._aggregatefunc = aggregation.toAggregateFunc(func);
    } else {
      return this._aggregatefunc;
    }
  }

  formatFunc(func) {
    if (func) {
      this._formatfunc = func;
    } else {
      return this._formatfunc;
    }
  }

}

const getpropertyvalue = (property, configs, defaultvalue) => {
  for (let i = 0; i < configs.length; i++) {
    if (configs[i][property] != null) {
      return configs[i][property];
    }
  }
  return defaultvalue;
};

const mergefieldconfigs = (...args) => {
  const merged = {
    configs: [],
    sorts: [],
    subtotals: [],
    functions: [],
  };

  for (let i = 0; i < args.length; i++) {
    const nnconfig = args[i] || {};
    merged.configs.push(nnconfig);
    merged.sorts.push(nnconfig.sort || {});
    merged.subtotals.push(nnconfig.subTotal || {});
    merged.functions.push({
      aggregateFuncName: nnconfig.aggregateFuncName,
      aggregateFunc: i === 0 ? nnconfig.aggregateFunc : (nnconfig.aggregateFunc ? nnconfig.aggregateFunc() : null),
      formatFunc: i === 0 ? nnconfig.formatFunc : (nnconfig.formatFunc ? nnconfig.formatFunc() : null),
    });
  }

  return merged;
};

const createfield = (rootconfig, axetype, fieldconfig, defaultfieldconfig) => {
  let axeconfig;
  let fieldAxisconfig;

  if (defaultfieldconfig) {
    switch (axetype) {
      case AxisType.ROWS:
        axeconfig = rootconfig.rowSettings;
        fieldAxisconfig = defaultfieldconfig.rowSettings;
        break;
      case AxisType.COLUMNS:
        axeconfig = rootconfig.columnSettings;
        fieldAxisconfig = defaultfieldconfig.columnSettings;
        break;
      case AxisType.DATA:
        axeconfig = rootconfig.dataSettings;
        fieldAxisconfig = defaultfieldconfig.dataSettings;
        break;
      default:
        axeconfig = null;
        fieldAxisconfig = null;
        break;
    }
  } else {
    axeconfig = null;
    fieldAxisconfig = null;
  }

  const merged = mergefieldconfigs(fieldconfig, fieldAxisconfig, axeconfig, defaultfieldconfig, rootconfig);

  return new Field({
    name: getpropertyvalue('name', merged.configs, ''),

    caption: getpropertyvalue('caption', merged.configs, ''),

    code: getpropertyvalue('code', merged.configs, ''),

    sort: {
      order: getpropertyvalue('order', merged.sorts, null),
      customfunc: getpropertyvalue('customfunc', merged.sorts, null),
    },
    subTotal: {
      visible: getpropertyvalue('visible', merged.subtotals, true),
      collapsible: getpropertyvalue('collapsible', merged.subtotals, true),
      collapsed: getpropertyvalue('collapsed', merged.subtotals, false) && getpropertyvalue('collapsible', merged.subtotals, true),
    },

    aggregateFuncName: getpropertyvalue('aggregateFuncName', merged.functions, 'sum'),
    aggregateFunc: getpropertyvalue('aggregateFunc', merged.functions, aggregation.sum),
    formatFunc: getpropertyvalue('formatFunc', merged.functions, null),
  }, false);
};

/**
 * Creates a new instance of pgrid config
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
// module.config(config) {
export class Config {

  constructor(config) {
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

    this.allFields = (config.fields || []).map(fieldConfig => new Field(fieldConfig));
    this.dataFields = (config.dataFields || []).map(fieldConfig => new Field(fieldConfig));

    // map fields names to captions
    this.dataFieldNames = this.allFields.map(f => f.name);
    this.dataFieldCaptions = this.allFields.map(f => f.caption);

    this.rowFields = (config.rows || []).map((fieldconfig) => {
      fieldconfig = this.ensureFieldConfig(fieldconfig);
      return createfield(this, AxisType.ROWS, fieldconfig, this.getfield(this.allFields, fieldconfig.name));
    });

    this.columnFields = (config.columns || []).map((fieldconfig) => {
      fieldconfig = this.ensureFieldConfig(fieldconfig);
      return createfield(this, AxisType.COLUMNS, fieldconfig, this.getfield(this.allFields, fieldconfig.name));
    });

    this.selectedField = this.allFields.filter(field => field.caption === config.rows[0])[0];

    this.activatedDataFields = this.dataFields.filter(field => config.data.indexOf(field.caption) > -1);

    this.drilldown = config.drilldown || (cell => console.log('drilldown on cell (default)', cell));

    this.runtimeVisibility = {
      subtotals: {
        rows: this.rowSettings.subTotal.visible !== undefined ? this.rowSettings.subTotal.visible : true,
        columns: this.columnSettings.subTotal.visible !== undefined ? this.columnSettings.subTotal.visible : true,
      },
    };
    this.activatedDataFieldsCount = this.getactivatedDataFieldsCount();
    this.availableFields = this.getavailableFields();
    this.preFilters = this.getpreFilters();
  }

  getactivatedDataFieldsCount() {
    return this.activatedDataFields ? this.activatedDataFields.length : 0;
  }

  getavailableFields() {
    const usedFields = [].concat(this.rowFields, this.columnFields);
    return this.allFields
      .filter(field => usedFields.map(field => field.name).indexOf(field.name) === -1);
  }

  getpreFilters() {
    const prefilters = {};
    if (this.config.preFilters) {
      Object.keys(this.config.preFilters).forEach((filteredField) => {
        const filterName = this.captionToName(filteredField);
        const prefilterConfig = this.config.preFilters[filteredField];
        if (Array.isArray(prefilterConfig)) {
          prefilters[filterName] = new ExpressionFilter(
            filterName,
            this.data,
            null,
            null,
            prefilterConfig,
            false,
          );
        } else {
          const opname = Object.keys(prefilterConfig)[0];
          if (opname) {
            prefilters[filterName] = new ExpressionFilter(
              filterName,
              this.data,
              opname,
              prefilterConfig[opname],
            );
          }
        }
      });
    }
    return prefilters;
  }

  captionToName(caption) {
    const fcaptionIndex = this.dataFieldCaptions.indexOf(caption);
    return fcaptionIndex >= 0 ? this.dataFieldNames[fcaptionIndex] : caption;
  }

  nameToCaption(name) {
    const fnameIndex = this.dataFieldNames.indexOf(name);
    return fnameIndex >= 0 ? this.dataFieldCaptions[fnameIndex] : name;
  }
  // setTheme (newTheme) {
  //   return this.theme.current() !== this.theme.current(newTheme)
  // }

  ensureFieldConfig(obj) {
    if (typeof obj === 'string') {
      return {
        name: this.captionToName(obj),
      };
    }
    return obj;
  }

  getfield(axefields, fieldname) {
    const fieldindex = this.getfieldindex(axefields, fieldname);
    if (fieldindex > -1) {
      return axefields[fieldindex];
    }
    return null;
  }

  getfieldindex(axefields, fieldname) {
    for (let fi = 0; fi < axefields.length; fi++) {
      if (axefields[fi].name === fieldname) {
        return fi;
      }
    }
    return -1;
  }

  getField(fieldname) {
    return this.getfield(this.allFields, fieldname);
  }

  getRowField(fieldname) {
    return this.getfield(this.rowFields, fieldname);
  }

  getColumnField(fieldname) {
    return this.getfield(this.columnFields, fieldname);
  }

  getDataField(fieldname) {
    return this.getfield(this.dataFields, fieldname);
  }

  moveField(fieldname, oldaxetype, newaxetype, position) {
    let oldaxe,
      oldposition;
    let newaxe;
    let fieldConfig;
    const defaultFieldConfig = this.getfield(this.allFields, fieldname);

    if (defaultFieldConfig) {
      switch (oldaxetype) {
        case AxisType.ROWS:
          oldaxe = this.rowFields;
          break;
        case AxisType.COLUMNS:
          oldaxe = this.columnFields;
          break;
        case AxisType.FIELDS:
          oldaxe = this.availableFields;
          break;
        default:
          break;
      }

      switch (newaxetype) {
        case AxisType.ROWS:
          newaxe = this.rowFields;
          fieldConfig = this.getRowField(fieldname);
          break;
        case AxisType.COLUMNS:
          newaxe = this.columnFields;
          fieldConfig = this.getColumnField(fieldname);
          break;
        case AxisType.FIELDS:
          newaxe = this.availableFields;
          fieldConfig = this.getField(fieldname);
          break;
        default:
          break;
      }

      if (oldaxe || newaxe) {
        const newAxisSubtotalsState = this.areSubtotalsVisible(newaxetype);

        if (oldaxe) {
          oldposition = this.getfieldindex(oldaxe, fieldname);
          if (oldaxetype === newaxetype) {
            if (oldposition === (oldaxe.length - 1 && position == null) ||
              oldposition === position - 1) {
              return false;
            }
          }
          oldaxe.splice(oldposition, 1);
        }

        const field = createfield(
          this,
          newaxetype,
          fieldConfig,
          defaultFieldConfig);

        if (!newAxisSubtotalsState && field.subTotal.visible !== false) {
          field.subTotal.visible = null;
        }

        if (newaxe) {
          if (position != null) {
            newaxe.splice(position, 0, field);
          } else {
            newaxe.push(field);
          }
        }
        if (newaxetype === AxisType.FIELDS) {
          return oldaxetype;
        } else if (oldaxetype === AxisType.FIELDS) {
          return newaxetype;
        }
      }
    }
    return -1;
  }

  selectField(fieldname) {
    this.selectedField = this.getfield(this.allFields, fieldname);
  }

  toggleDataField(fieldname) {
    const defaultFieldConfig = this.getDataField(fieldname);
    const newDataFields = this.activatedDataFields.filter(fld => fld.name !== fieldname);
    if (this.activatedDataFields.length === newDataFields.length) {
      this.activatedDataFields.push(defaultFieldConfig);
    } else {
      this.activatedDataFields = newDataFields;
    }
    this.activatedDataFieldsCount = this.getactivatedDataFieldsCount();
    return this.activatedDataFieldsCount;
  }

  toggleSubtotals(axetype) {
    let i;
    let axeFields;
    let newState = !this.areSubtotalsVisible(axetype);

    if (axetype === AxisType.ROWS) {
      this.runtimeVisibility.subtotals.rows = newState;
      axeFields = this.rowFields;
    } else if (axetype === AxisType.COLUMNS) {
      this.runtimeVisibility.subtotals.columns = newState;
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

  areSubtotalsVisible(axetype) {
    if (axetype === AxisType.ROWS) {
      return this.runtimeVisibility.subtotals.rows;
    } else if (axetype === AxisType.COLUMNS) {
      return this.runtimeVisibility.subtotals.columns;
    } else {
      return null;
    }
  }

  toggleGrandtotal(axetype) {
    const newState = !this.isGrandtotalVisible(axetype);

    if (axetype === AxisType.ROWS) {
      this.grandTotal.rowsvisible = newState;
    } else if (axetype === AxisType.COLUMNS) {
      this.grandTotal.columnsvisible = newState;
    } else {
      return false;
    }
    return true;
  }

  isGrandtotalVisible(axetype) {
    if (axetype === AxisType.ROWS) {
      return this.grandTotal.rowsvisible;
    } else if (axetype === AxisType.COLUMNS) {
      return this.grandTotal.columnsvisible;
    } else {
      return false;
    }
  }
}
