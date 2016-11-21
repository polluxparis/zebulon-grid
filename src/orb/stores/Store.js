import { Observable } from 'rx-lite';

import { Axis, AxisType } from '../Axis';
import AxisUi from '../AxisUi';
import { Config } from '../Config';
import { ExpressionFilter } from '../Filtering';
import * as utils from '../Utils';

export const MEASURE_ID = '__measures__';
export const TOTAL_ID = '__total__';

/**
 * Creates a new instance of store
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
export default class Store {

  constructor(config, forceUpdateCallback, datasource) {
    this.init = false;
    this.defaultfield = { name: '#undefined#' };
    // If no forceUpdate function is passed, use a mock
    // Useful for tests
    this.forceUpdateCallback = forceUpdateCallback || function mockForceUpdate() {};
    this.config = new Config(config);
    this.filters = new Map();
    Object.keys(this.config.preFilters)
    .forEach(key => this.filters.set(key, this.config.preFilters[key]));
    this.zoom = 1;
    this.rowsUi = this.getrowsUi();
    this.columnsUi = this.getcolumnsUi();
    this.layout = this.getlayout();
    this.rowHeaderSizes = { leafs: {}, dimensions: {} };
    this.columnHeaderSizes = { leafs: {}, dimensions: {} };
    this.getsizes(true);

    this.init = true;
    if (datasource) this.subscribe(datasource);
  }

  subscribe(datasource) {
    this.data = [];
    this.filteredData = [];
    let observableDatasource = null;
    // datasource can be an observable, an array of arrays or an array of objects
    if (Array.isArray(datasource) && (Array.isArray(datasource[0]) || typeof datasource[0] === 'object')) {
      observableDatasource = Observable.of(datasource);
    } else if (Observable.isObservable(datasource)) {
      // datasource is a Rxjs observable
      observableDatasource = datasource;
    }
    if (observableDatasource) {
      this.dataSubscription = observableDatasource.subscribe(this.push.bind(this));
    }
  }

  unsubscribe() {
    if (this.dataSubscription) {
      this.dataSubscription.dispose();
    }
  }


// /////////////////////////////////////////////////////////
// /////////////////////// ACTIONS //////////////////////////
// /////////////////////////////////////////////////////////

  push(payload) {
    let pushed;
    const data = this.data;
    // Push data(array of objects, array of arrays or object) to this.data
    // New data is pushed at the top(using unshift) in order to read the newest data first
    // when building the axes
    if (Array.isArray(payload) && (Array.isArray(payload[0]) || typeof payload[0] === 'object')) {
      payload.forEach((line) => { data.unshift(line); });
      pushed = payload;
    } else if (Array.isArray(payload) || typeof payload === 'object') {
      data.unshift(payload);
      pushed = [payload];
    }
    // Push filtered data and refresh Ui
    if (pushed) {
      const filteredPush = this.filter(pushed);
      if (filteredPush.length) {
        filteredPush.forEach((line) => { this.filteredData.unshift(line); });
        this.columnsUi = this.getcolumnsUi();
        this.rowsUi = this.getrowsUi();
        this.layout = this.getlayout();
        this.getsizes();
        if (this.init) { this.forceUpdateCallback(); }
      }
    }
    this.data = data;
  }

  handleZoom(zoomIn) {
    const { zoom } = this;
    const zoomValues = [0.25, 0.33, 0.5, 0.67, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 1.75, 2, 2.5, 3, 4, 5];
    const zoomIndex = zoomValues.indexOf(zoom);
    let nextZoomIndex;
    if (zoomIn) {
      nextZoomIndex = Math.min(zoomIndex + 1, zoomValues.length - 1);
    } else {
      nextZoomIndex = Math.max(zoomIndex - 1, 0);
    }
    this.zoom = zoomValues[nextZoomIndex];
    this.getsizes(true);
    this.forceUpdateCallback();
  }

  sort(axetype, fieldName) {
    let sorted = false;
    if (axetype === AxisType.ROWS) {
      this.rows.sort(fieldName);
      sorted = true;
    } else if (axetype === AxisType.COLUMNS) {
      this.columns.sort(fieldName);
      sorted = true;
    }
    if (sorted && this.init) {
      switch (axetype) {
        case AxisType.ROWS:
          this.rowsUi = this.getrowsUi(true);
          break;
        case AxisType.COLUMNS:
          this.columnsUi = this.getcolumnsUi(true);
          break;
        default:
          break;
      }
      this.forceUpdateCallback();
    }
  }

  moveField(fieldName, oldAxisType, newAxisType, position) {
    const axisType = this.config.moveField(fieldName, oldAxisType, newAxisType, position);
    switch (axisType) {
      case AxisType.COLUMNS:
        this.columnsUi = this.getcolumnsUi();
        if (!this.config.rowFields.length) {
          // If the other axis has no field,
          // it must be recreated to update the key of the Total header
          // Since there are no field, it is very cheap anyway
          this.rowsUi = this.getrowsUi();
        }
        break;
      case AxisType.ROWS:
        this.rowsUi = this.getrowsUi();
        if (!this.config.columnFields.length) {
          // If the other axis has no field,
          // it must be recreated to update the key of the Total header
          // Since there are no field, it is very cheap anyway
          this.columnsUi = this.getcolumnsUi();
        }
        break;
      default:
        this.columnsUi = this.getcolumnsUi();
        this.rowsUi = this.getrowsUi();
    }
    this.layout = this.getlayout();
    this.getsizes();
    this.forceUpdateCallback();
  }

  selectField(fieldName) {
    this.config.selectField(fieldName);
    this.rows = this.getrows();
    this.forceUpdateCallback();
  }

  toggleDataField(fieldname) {
    // toggleDataField returns the count of activated data fields.
    // If it is 0, there is no need to recompute the axes
    // as the only effect is to make the data cells blank.
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
      this.getsizes();
    }
    this.forceUpdateCallback();
  }

  applyFilter(fieldname, axetype, all, operator, term, staticValue, excludeStatic) {
    this.addFilter(fieldname, axetype, all, operator, term, staticValue, excludeStatic);
    this.filteredData = this.filter(this.data);
    this.columnsUi = this.getcolumnsUi();
    this.rowsUi = this.getrowsUi();
    this.layout = this.getlayout();
    this.getsizes();
    this.forceUpdateCallback();
  }

  addFilter(fieldname, axetype, all, operator, term, staticValue, excludeStatic) {
    if (all && this.filters.has(fieldname)) {
      this.filters.delete(fieldname);
    } else if (!all) {
      this.filters.set(fieldname,
        new ExpressionFilter(
          fieldname,
          this.filteredData,
          operator,
          term,
          staticValue,
          excludeStatic,
        ));
    }
  }

  filter(data) {
    const filters = [...this.filters.values()];
    if (filters.length === 0) {
      return data;
    }
    return data.filter(row =>
      filters.every(filter => filter.pass(row[filter.fieldName])));
  }

  refreshData(data) {
    this.filteredData = data;
  }

  updateCellSizes(handle, offset, initialOffset) {
    function updateCellSize(size, offset) { return Math.max(size + offset, 10); }
    if (handle.isOnDimensionHeader) {
      if (handle.axis === AxisType.COLUMNS) {
        this.columnHeaderSizes.dimensions[handle.id] = updateCellSize(
          this.columnHeaderSizes.dimensions[handle.id] || this.sizes.cellHeight,
          offset.y - initialOffset.y);
      } else {
        this.rowHeaderSizes.dimensions[handle.id] = updateCellSize(
          this.rowHeaderSizes.dimensions[handle.id] || this.sizes.cellWidth,
          offset.x - initialOffset.x);
      }
    } else if (handle.axis === AxisType.COLUMNS && handle.position === 'right') {
      if (handle.leafSubheaders.length) {
        const fractionalOffset = (offset.x - initialOffset.x) / handle.leafSubheaders.length;
        handle.leafSubheaders.forEach((subheader) => {
          this.columnHeaderSizes.leafs[subheader.key] = updateCellSize(
              this.columnHeaderSizes.leafs[subheader.key] || this.sizes.cellWidth,
              fractionalOffset);
        });
      } else {
          // Header is a leaf header
        this.columnHeaderSizes.leafs[handle.id] = updateCellSize(
            this.columnHeaderSizes.leafs[handle.id] || this.sizes.cellWidth,
            offset.x - initialOffset.x);
      }
    } else if (handle.axis === AxisType.ROWS && handle.position === 'bottom') {
      if (handle.leafSubheaders.length) {
        const fractionalOffset = (offset.y - initialOffset.y) / handle.leafSubheaders.length;
        handle.leafSubheaders.forEach((subheader) => {
          this.rowHeaderSizes.leafs[subheader.key] = updateCellSize(
              this.rowHeaderSizes.leafs[subheader.key] || this.sizes.cellHeight,
              fractionalOffset);
        });
      } else {
          // Header is a leaf header
        this.rowHeaderSizes.leafs[handle.id] = updateCellSize(
            this.rowHeaderSizes.leafs[handle.id] || this.sizes.cellHeight,
            offset.y - initialOffset.y);
      }
    } else if (handle.axis === AxisType.COLUMNS && handle.position === 'bottom') {
      this.columnHeaderSizes.dimensions[handle.id] = updateCellSize(
          this.columnHeaderSizes.dimensions[handle.id] || this.sizes.cellHeight,
          offset.y - initialOffset.y);
    } else if (handle.axis === AxisType.ROWS && handle.position === 'right') {
      this.rowHeaderSizes.dimensions[handle.id] = updateCellSize(
          this.rowHeaderSizes.dimensions[handle.id] || this.sizes.cellWidth,
          offset.x - initialOffset.x);
    }
    this.getsizes();
    this.forceUpdateCallback();
  }


  // toggleSubtotals(axetype) {
  //   if (this.config.toggleSubtotals(axetype)) {
  //   }
  // }
  //
  // toggleGrandtotal(axetype) {
  //   if (this.config.toggleGrandtotal(axetype)) {
  //   }
  // }

// /////////////////////////////////////////////////////////
// /////////////////////// VALUES //////////////////////////
// /////////////////////////////////////////////////////////

  getrows() {
    return new Axis(AxisType.ROWS, this.config.rowFields, this.filteredData);
  }

  getcolumns() {
    return new Axis(AxisType.COLUMNS, this.config.columnFields, this.filteredData);
  }

  getChartAxis() {
    return new Axis(AxisType.CHART, [this.config.selectedField], this);
  }

  getrowsUi(noNewAxis) {
    if (!noNewAxis) { this.rows = this.getrows(); }
    return new AxisUi(this.rows, this.config);
  }

  getcolumnsUi(noNewAxis) {
    if (!noNewAxis) { this.columns = this.getcolumns(); }
    return new AxisUi(this.columns, this.config);
  }

  getlayout() {
    const rowHorizontalCount = (this.rows.fields.length || 1) +
      (this.config.dataHeadersLocation === 'rows' && this.config.activatedDataFieldsCount >= 1 ? 1 : 0);
    const rowVerticalCount = this.rowsUi.headers.length;
    const columnHorizontalCount = this.columnsUi.headers.length;
    const columnVerticalCount = (this.columns.fields.length || 1) +
      (this.config.dataHeadersLocation === 'columns' && this.config.activatedDataFieldsCount >= 1 ? 1 : 0);
    return { rowHorizontalCount, rowVerticalCount, columnHorizontalCount, columnVerticalCount };
  }

  getsizes(cellSizeChange) {
    if (cellSizeChange) {
      this.sizes = {};
      this.getCellSizes();
    }
    this.getHeaderSizes();
    this.getDimensionPositions();
  }

  getCellSizes() {
    this.sizes.cellHeight = this.zoom * (this.config.cellHeight || 30);
    this.sizes.cellWidth = this.zoom * (this.config.cellWidth || 100);
  }

  getHeaderSizes() {
    const columnsMeasures = this.config.dataHeadersLocation === 'columns';
    let rowHeadersWidth = 0;
    // Measures are on the row axis
    if (!columnsMeasures) {
      rowHeadersWidth += this.rowHeaderSizes.dimensions[MEASURE_ID] || this.sizes.cellWidth;
    }
    // There are no fields on the row axis
    if (!this.rows.fields.length) {
      rowHeadersWidth += this.getDimensionSize(AxisType.ROWS, TOTAL_ID);
    } else {
      rowHeadersWidth = this.rows.fields.reduce(
        (width, field) => width + this.getDimensionSize(AxisType.ROWS, field.code),
        rowHeadersWidth);
    }
    let columnHeadersHeight = 0;
    // Measures are on the column axis
    if (columnsMeasures) {
      columnHeadersHeight += this.columnHeaderSizes.dimensions[MEASURE_ID] || this.sizes.cellHeight;
    }
    // There are no fields on the column axis
    if (!this.columns.fields.length) {
      columnHeadersHeight += this.getDimensionSize(AxisType.COLUMNS, TOTAL_ID);
    } else {
      columnHeadersHeight = this.columns.fields.reduce(
        (height, field) => height + this.getDimensionSize(AxisType.COLUMNS, field.code),
        columnHeadersHeight);
    }
    const rowHeadersHeight = this.rowsUi.headers.reduce(
      (height, headers) => height + this.getRowHeight({ index: headers[0].x }),
      0);
    const columnHeadersWidth = this.columnsUi.headers.reduce(
      (width, headers) => width + this.getColumnWidth({ index: headers[0].x }),
      0);
    this.sizes.rowHeadersWidth = rowHeadersWidth;
    this.sizes.rowHeadersHeight = rowHeadersHeight;
    this.sizes.columnHeadersHeight = columnHeadersHeight;
    this.sizes.columnHeadersWidth = columnHeadersWidth;
  }

  getDimensionPositions() {
    this.dimensionPositions = {};
    this.dimensionPositions.columns = this.calculateDimensionPositions('columns', this.config.columnFields, this.config.dataHeadersLocation === 'columns');
    this.dimensionPositions.rows = this.calculateDimensionPositions('rows', this.config.rowFields, this.config.dataHeadersLocation === 'rows');
  }

  calculateDimensionPositions(axis, fields, hasMeasures) {
    let axisType;
    if (axis === 'columns') {
      axisType = 1;
    } else {
      axisType = 2;
    }
    const res = {};
    let position = 0;
    // Total header if no fields
    if (!fields.length) {
      position += this.getDimensionSize(axisType, TOTAL_ID);
    } else {
      fields.forEach((field) => {
        res[field.code] = position;
        position += this.getDimensionSize(axisType, field.code);
      });
    }
    if (hasMeasures) {
      res[MEASURE_ID] = position;
    }
    return res;
  }

  getLeafHeaderSize(axis, key) {
    if (axis === AxisType.COLUMNS) {
      return this.columnHeaderSizes.leafs[key] || this.sizes.cellWidth;
    }
    return this.rowHeaderSizes.leafs[key] || this.sizes.cellHeight;
  }

  getLastChildSize(axis, header) {
    let lastChild = header;
    while (lastChild.subheaders && lastChild.subheaders.length) {
      lastChild = lastChild.subheaders[lastChild.subheaders.length - 1];
    }
    return this.getLeafHeaderSize(axis, header.key);
  }

  getDimensionSize(axis, code) {
    if (axis === AxisType.COLUMNS) {
      return this.columnHeaderSizes.dimensions[code] || this.sizes.cellHeight;
    }
    return this.rowHeaderSizes.dimensions[code] || this.sizes.cellWidth;
  }

  getColumnWidth({ index }) {
    // Need to pass the axis when the grid receives new props
    if (index >= this.columnsUi.headers.length) {
      // Because of offset, it's necessary to make column and row counts greater than normal
      // This ensures that the additional cells are correctly handled
      return this.sizes.rowHeadersWidth / this.layout.rowHorizontalCount;
    }
    const headers = this.columnsUi.headers[index];
    const key = headers[headers.length - 1].key;
    return this.getLeafHeaderSize(AxisType.COLUMNS, key);
  }

  getRowHeight({ index }) {
    // Need to pass the axis when the grid receives new props
    if (index >= this.rowsUi.headers.length) {
      // Because of offset, it's necessary to make column and row counts greater than normal
      // This ensures that the additional cells are correctly handled
      return this.sizes.columnHeadersHeight / this.layout.columnVerticalCount;
    }
    const headers = this.rowsUi.headers[index];
    const key = headers[headers.length - 1].key;
    return this.getLeafHeaderSize(AxisType.ROWS, key);
  }

  drilldown(cell) {
    return this.config.drilldown(cell);
  }

  areSubtotalsVisible(axetype) {
    return this.config.areSubtotalsVisible(axetype);
  }

  isGrandtotalVisible(axetype) {
    return this.config.isGrandtotalVisible(axetype);
  }

  getFieldValues(field, filterFunc) {
    const values1 = [];
    let values = [];
    let containsBlank = false;
    // We use data here instead of filteredData
    // Otherwise you lose the filtered values the next time you open a Filter Panel
    for (let i = 0; i < this.data.length; i += 1) {
      const row = this.data[i];
      const val = row[field];
      if (filterFunc !== undefined) {
        if (filterFunc === true || (typeof filterFunc === 'function' && filterFunc(val))) {
          values1.push(val);
        }
      } else if (val != null) {
        values1.push(val);
      } else {
        containsBlank = true;
      }
    }
    if (values1.length > 1) {
      if (utils.isNumber(values1[0]) || utils.isDate(values1[0])) {
        values1.sort((a, b) => {
          if (a) {
            if (b) {
              return a - b;
            }
            return 1;
          }
          if (b) {
            return -1;
          }
          return 0;
        });
      } else {
        values1.sort();
      }

      for (let vi = 0; vi < values1.length; vi += 1) {
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

  // isFieldFiltered(field) {
  //   const filter = this.filters.get(field);
  //   return filter != null && !filter.isAlwaysTrue();
  // }

  getData(field, rowdim, coldim, aggregateFunc) {
    let value;
    if (rowdim && coldim) {
      const datafieldName = field || (this.config.activatedDataFields[0] || this.defaultfield).name;
      value = this.calcAggregation(
        rowdim.isRoot ? null : rowdim.getRowIndexes().slice(0),
        coldim.isRoot ? null : coldim.getRowIndexes().slice(0),
        [datafieldName],
        aggregateFunc)[datafieldName];
    }
    return value === undefined ? null : value;
  }

  calcAggregation(rowIndexes, colIndexes, fieldNames, aggregateFunc) {
    const res = {};

    if (this.config.activatedDataFieldsCount > 0) {
      let intersection;

      if (rowIndexes === null && colIndexes === null) {
        // At initialization, both rowIndexes and colIndexes are null
        intersection = null;
      } else if (rowIndexes === null) {
        intersection = colIndexes;
      } else if (colIndexes === null) {
        intersection = rowIndexes;
      } else {
        intersection = utils.twoArraysIntersect(colIndexes, rowIndexes);
      }

      const emptyIntersection = !intersection || intersection.length === 0;
      const data = this.filteredData;
      let datafield;
      const datafields = [];

      if (fieldNames) {
        for (let fieldnameIndex = 0; fieldnameIndex < fieldNames.length; fieldnameIndex += 1) {
          datafield = this.config.getDataField(fieldNames[fieldnameIndex]);
          if (!aggregateFunc) {
            if (!datafield) {
              datafield = this.config.getField(fieldNames[fieldnameIndex]);
              if (datafield) {
                aggregateFunc = datafield.dataSettings
                ? datafield.dataSettings.aggregateFunc()
                : datafield.aggregateFunc();
              }
            } else {
              aggregateFunc = datafield.aggregateFunc();
            }
          }

          if (datafield && aggregateFunc) {
            datafields.push({ field: datafield, aggregateFunc });
          }
        }
      } else {
        for (
          let datafieldIndex = 0;
          datafieldIndex < this.config.activatedDataFieldsCount;
          datafieldIndex += 1
        ) {
          datafield = this.config.activatedDataFields[datafieldIndex] || this.defaultfield;
          if (aggregateFunc || datafield.aggregateFunc) {
            datafields.push({
              field: datafield,
              aggregateFunc: aggregateFunc || datafield.aggregateFunc(),
            });
          }
        }
      }

      for (let dfi = 0; dfi < datafields.length; dfi += 1) {
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
}
