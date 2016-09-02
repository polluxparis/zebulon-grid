'use strict'
import { Observable } from 'rx-lite'

import { Axe, AxeType } from '../Axe'
import AxeUi from '../AxeUi'
import { Config } from '../Config'
import { ExpressionFilter } from '../Filtering'
import * as utils from '../Utils'

/**
 * Creates a new instance of store
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
export default class Store {

  constructor (config, forceUpdateCallback) {
    this.init = false
    this.dataMatrix = {}
    this.defaultfield = { name: '#undefined#' }
    this.forceUpdateCallback = forceUpdateCallback
    this.config = new Config(config)
    this.filters = new Map()
    Object.keys(this.config.preFilters).forEach(key => this.filters.set(key, this.config.preFilters[key]))
    this.sizes = this.getsizes()
    this.rowsUi = this.getrowsUi()
    this.columnsUi = this.getcolumnsUi()
    this.layout = this.getlayout()
    this.init = true
  }

  subscribe (datasource) {
    this.data = []
    this.filteredData = []
    let observableDatasource = null
    // datasource can be an observable, an array of arrays or an array of objects
    if (Array.isArray(datasource) && (Array.isArray(datasource[0]) || typeof datasource[0] === 'object')) {
      observableDatasource = Observable.of(datasource)
    } else if (Observable.isObservable(datasource)) {
      // datasource is a Rxjs observable
      observableDatasource = datasource
    }
    if (observableDatasource) observableDatasource.subscribe(this.push.bind(this))
  }

  push (payload) {
    let pushed
    let _data = this.data
    console.log(this.filteredData)
    console.log([...this.data])
    console.log(payload)
    // Push data (array of objects, array of arrays or object) to this.data
    if (Array.isArray(payload) && (Array.isArray(payload[0]) || typeof payload[0] === 'object')) {
      payload.forEach(line => { _data.push(line) })
      pushed = payload
    } else if (Array.isArray(payload) || typeof payload === 'object') {
      _data.push(payload)
      pushed = [payload]
    }
    // Push filtered data and refresh Ui
    if (pushed) {
      const filteredPush = this.filter(pushed)
      if (filteredPush.length) {
        filteredPush.forEach(line => { this.filteredData.push(line) })
        this.columnsUi = this.getcolumnsUi()
        this.rowsUi = this.getrowsUi()
        this.layout = this.getlayout()
        if (this.init) { this.forceUpdateCallback() }
      }
    }
    this.data = _data
    console.log([...this.data])
  }

  filter (data) {
    let filterFields = [...this.filters.keys()]
    if (filterFields.length > 0) {
      const res = []

      for (let i = 0; i < data.length; i++) {
        let row = data[i]
        let exclude = false
        for (let fi = 0; fi < filterFields.length; fi++) {
          let fieldname = filterFields[fi]
          let fieldFilter = this.filters.get(fieldname)

          if (fieldFilter && !fieldFilter.test(row[fieldname])) {
            exclude = true
            break
          }
        }
        if (!exclude) {
          res.push(row)
        }
      }
      return res
    } else {
      return data
    }
  }

  getrows () {
    return new Axe(AxeType.ROWS, this.config.rowFields, this)
  }

  getcolumns () {
    return new Axe(AxeType.COLUMNS, this.config.columnFields, this)
  }

  getrowsUi (noNewAxe) {
    if (!noNewAxe) { this.rows = this.getrows() }
    return new AxeUi(this.rows)
  }

  getcolumnsUi (noNewAxe) {
    if (!noNewAxe) { this.columns = this.getcolumns() }
    return new AxeUi(this.columns)
  }

  getlayout () {
    const rowHeaders = {
      width: (this.rows.fields.length || 1) +
        (this.config.dataHeadersLocation === 'rows' && this.config.activatedDataFieldsCount > 1 ? 1 : 0),
      height: this.rowsUi.headers.length
    }
    const columnHeaders = {
      width: this.columnsUi.headers.length,
      height: (this.columns.fields.length || 1) +
        (this.config.dataHeadersLocation === 'columns' && this.config.activatedDataFieldsCount > 1 ? 1 : 0)
    }
    const pivotTable = {
      width: rowHeaders.width + columnHeaders.width,
      height: rowHeaders.height + columnHeaders.height
    }
    return {columnHeaders, rowHeaders, pivotTable}
  }

  getsizes () {
    const cell = {
      height: 30,
      width: 100
    }
    const grid = {
      width: this.config.width,
      height: this.config.height
    }
    return {cell, grid}
  }

  sort (axetype, field) {
    let sorted = false
    if (axetype === AxeType.ROWS) {
      this.rows.sort(field)
      sorted = true
    } else if (axetype === AxeType.COLUMNS) {
      this.columns.sort(field)
      sorted = true
    }
    if (sorted && this.init) {
      switch (axetype) {
        case AxeType.ROWS:
          this.rowsUi = this.getrowsUi(true)
          break
        case AxeType.COLUMNS:
          this.columnsUi = this.getcolumnsUi(true)
          break
        default:
          break
      }
      this.forceUpdateCallback()
    }
  }

  moveField (fieldname, oldaxetype, newaxetype, position) {
    const axeType = this.config.moveField(fieldname, oldaxetype, newaxetype, position)
    switch (axeType) {
      case AxeType.COLUMNS:
        this.columnsUi = this.getcolumnsUi()
        break
      case AxeType.ROWS:
        this.rowsUi = this.getrowsUi()
        break
      default:
        this.columnsUi = this.getcolumnsUi()
        this.rowsUi = this.getrowsUi()
    }
    this.layout = this.getlayout()
    this.forceUpdateCallback()
  }

  toggleDataField (fieldname) {
    // toggleDataField returns the count of activated data fields.
    // If it is 0, there is no need to recompute the axes as the only effect is to make the data cells blank.
    if (this.config.toggleDataField(fieldname)) {
      switch (this.config.dataHeadersLocation) {
        case 'columns':
          this.columnsUi = this.getcolumnsUi()
          break
        case 'rows':
          this.rowsUi = this.getrowsUi()
          break
        default:
          break
      }
      this.layout = this.getlayout()
    }
    this.forceUpdateCallback()
  }

  applyFilter (fieldname, axetype, all, operator, term, staticValue, excludeStatic) {
    if (all && this.filters.has(fieldname)) {
      this.filters.delete(fieldname)
    } else if (!all) {
      this.filters.set(fieldname, new ExpressionFilter(fieldname, this.filteredData, operator, term, staticValue, excludeStatic))
    }
    this.filteredData = this.filter(this.data)
    this.columnsUi = this.getcolumnsUi()
    this.rowsUi = this.getrowsUi()
    this.layout = this.getlayout()
    this.forceUpdateCallback()
  }

  drilldown (cell) {
    return this.config.drilldown(cell)
  }

  refreshData (data) {
    this.filteredData = data
  }

  toggleSubtotals (axetype) {
    if (this.config.toggleSubtotals(axetype)) {
    }
  }

  toggleGrandtotal (axetype) {
    if (this.config.toggleGrandtotal(axetype)) {
    }
  }

  areSubtotalsVisible (axetype) {
    return this.config.areSubtotalsVisible(axetype)
  }

  isGrandtotalVisible (axetype) {
    return this.config.isGrandtotalVisible(axetype)
  }

  getFieldValues (field, filterFunc) {
    let values1 = []
    let values = []
    let containsBlank = false
    // We use config.data here instead of filteredData because otherwise you lose the filtered values the next time you open a Filter Panel
    for (let i = 0; i < this.data.length; i++) {
      let row = this.data[i]
      let val = row[field]
      if (filterFunc !== undefined) {
        if (filterFunc === true || (typeof filterFunc === 'function' && filterFunc(val))) {
          values1.push(val)
        }
      } else {
        if (val != null) {
          values1.push(val)
        } else {
          containsBlank = true
        }
      }
    }
    if (values1.length > 1) {
      if (utils.isNumber(values1[0]) || utils.isDate(values1[0])) {
        values1.sort(function (a, b) { return a ? (b ? a - b : 1) : (b ? -1 : 0) })
      } else {
        values1.sort()
      }

      for (let vi = 0; vi < values1.length; vi++) {
        if (vi === 0 || values1[vi] !== values[values.length - 1]) {
          values.push(values1[vi])
        }
      }
    } else {
      values = values1
    }
    if (containsBlank) {
      values.unshift(null)
    }
    return values
  }

  getFieldFilter (field) {
    return this.filters[field]
  }

  isFieldFiltered (field) {
    let filter = this.getFieldFilter(field)
    return filter != null && !filter.isAlwaysTrue()
  }

  getData (field, rowdim, coldim, aggregateFunc) {
    let value
    if (rowdim && coldim) {
      const datafieldName = field || (this.config.activatedDataFields[0] || this.defaultfield).name
      value = this.calcAggregation(
        rowdim.isRoot ? null : rowdim.getRowIndexes().slice(0),
        coldim.isRoot ? null : coldim.getRowIndexes().slice(0),
        [datafieldName],
        aggregateFunc
      )[datafieldName]
    }
    return value === undefined ? null : value
  }

  calcAggregation (rowIndexes, colIndexes, fieldNames, aggregateFunc) {
    let res = {}

    if (this.config.activatedDataFieldsCount > 0) {
      let intersection

      if (rowIndexes === null) {
        intersection = colIndexes
      } else if (colIndexes === null) {
        intersection = rowIndexes
      } else {
        intersection = utils.twoArraysIntersect(colIndexes, rowIndexes)
      }

      const emptyIntersection = intersection && intersection.length === 0
      const data = this.filteredData
      let datafield
      let datafields = []

      if (fieldNames) {
        for (let fieldnameIndex = 0; fieldnameIndex < fieldNames.length; fieldnameIndex++) {
          datafield = this.config.getDataField(fieldNames[fieldnameIndex])
          if (!aggregateFunc) {
            if (!datafield) {
              datafield = this.config.getField(fieldNames[fieldnameIndex])
              if (datafield) {
                aggregateFunc = datafield.dataSettings ? datafield.dataSettings.aggregateFunc() : datafield.aggregateFunc()
              }
            } else {
              aggregateFunc = datafield.aggregateFunc()
            }
          }

          if (datafield && aggregateFunc) {
            datafields.push({ field: datafield, aggregateFunc })
          }
        }
      } else {
        for (let datafieldIndex = 0; datafieldIndex < this.config.activatedDataFieldsCount; datafieldIndex++) {
          datafield = this.config.activatedDataFields[datafieldIndex] || this.defaultfield
          if (aggregateFunc || datafield.aggregateFunc) {
            datafields.push({ field: datafield, aggregateFunc: aggregateFunc || datafield.aggregateFunc() })
          }
        }
      }

      for (let dfi = 0; dfi < datafields.length; dfi++) {
        datafield = datafields[dfi]
        // no data
        if (emptyIntersection) {
          res[datafield.field.name] = null
        } else {
          res[datafield.field.name] = datafield.aggregateFunc(datafield.field.name, intersection || 'all', data, rowIndexes, colIndexes)
        }
      }
    }

    return res
  }
}
