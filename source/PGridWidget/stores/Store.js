'use strict'

import { observable, computed, action, asMap } from 'mobx'

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

  @observable filters = asMap({})
  @observable config = {}

  constructor (config) {
    this.defaultfield = { name: '#undefined#' }
    this.config = new Config(config)
    this.filters.merge(this.config.preFilters)
    this.dataMatrix = {}
  }

  @computed get rows () {
    return new Axe(AxeType.ROWS, this.config.rowFields, this)
  }

  @computed get columns () {
    return new Axe(AxeType.COLUMNS, this.config.columnFields, this)
  }

  @computed get rowsUi () {
    return new AxeUi(this.rows)
  }

  @computed get columnsUi () {
    return new AxeUi(this.columns)
  }

  @computed get layout () {
    const rowHeaders = {
      width: (this.rows.fields.length || 1) +
        (this.config.dataHeadersLocation === 'rows' && this.config.dataFieldsCount > 1 ? 1 : 0),
      height: this.rowsUi.headers.length
    }
    const columnHeaders = {
      width: this.columnsUi.headers.length,
      height: (this.columns.fields.length || 1) +
        (this.config.dataHeadersLocation === 'columns' && this.config.dataFieldsCount > 1 ? 1 : 0)
    }
    const pivotTable = {
      width: rowHeaders.width + columnHeaders.width,
      height: rowHeaders.height + columnHeaders.height
    }
    return {columnHeaders, rowHeaders, pivotTable}
  }

  @computed get sizes () {
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

  @action sort (axetype, field) {
    if (axetype === AxeType.ROWS) {
      this.rows.sort(field)
    } else if (axetype === AxeType.COLUMNS) {
      this.columns.sort(field)
    } else {
      return
    }
  }

  @action moveField (fieldname, oldaxetype, newaxetype, position) {
    this.config.moveField(fieldname, oldaxetype, newaxetype, position)
  }

  @action toggleDataField (fieldname) {
    if (this.config.toggleDataField(fieldname)) {
      return true
    } else {
      return false
    }
  }

  @action applyFilter (fieldname, all, operator, term, staticValue, excludeStatic) {
    if (all && this.filters.has(fieldname)) {
      this.filters.delete(fieldname)
    } else if (!all) {
      this.filters.set(fieldname, new ExpressionFilter(fieldname, operator, term, staticValue, excludeStatic, this.config.dataSource))
    }
  }

  @action refreshData (data) {
    this.config.dataSource = data
  }

  @action toggleSubtotals (axetype) {
    if (this.config.toggleSubtotals(axetype)) {
    }
  }

  @action toggleGrandtotal (axetype) {
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
    var values1 = []
    var values = []
    var containsBlank = false
    for (var i = 0; i < this.config.dataSource.length; i++) {
      var row = this.config.dataSource[i]
      var val = row[field]
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

      for (var vi = 0; vi < values1.length; vi++) {
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
    var filter = this.getFieldFilter(field)
    return filter != null && !filter.isAlwaysTrue()
  }

  getData (field, rowdim, coldim, aggregateFunc) {
    let value
    if (rowdim && coldim) {
      const datafieldName = field || (this.config.dataFields[0] || this.defaultfield).name
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
    var res = {}

    if (this.config.dataFieldsCount > 0) {
      var intersection

      if (rowIndexes === null) {
        intersection = colIndexes
      } else if (colIndexes === null) {
        intersection = rowIndexes
      } else {
        intersection = utils.twoArraysIntersect(colIndexes, rowIndexes)
      }

      var emptyIntersection = intersection && intersection.length === 0
      var datasource = this.config.dataSource
      var datafield
      var datafields = []

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
        for (let datafieldIndex = 0; datafieldIndex < this.config.dataFieldsCount; datafieldIndex++) {
          datafield = this.config.dataFields[datafieldIndex] || this.defaultfield
          if (aggregateFunc || datafield.aggregateFunc) {
            datafields.push({ field: datafield, aggregateFunc: aggregateFunc || datafield.aggregateFunc() })
          }
        }
      }

      for (var dfi = 0; dfi < datafields.length; dfi++) {
        datafield = datafields[dfi]
        // no data
        if (emptyIntersection) {
          res[datafield.field.name] = null
        } else {
          res[datafield.field.name] = datafield.aggregateFunc(datafield.field.name, intersection || 'all', datasource, rowIndexes, colIndexes)
        }
      }
    }

    return res
  }
}
