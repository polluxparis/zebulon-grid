'use strict'

import { observable } from 'mobx'

import { PubSub } from '../Pubsub'
import { Axe, AxeType } from '../Axe'
import { Config } from '../Config'
import { ExpressionFilter } from '../Filtering'
import * as utils from '../Utils'

// pgrid events
export const EVENT_UPDATED = 'pgrid:updated'
export const EVENT_COLUMNS_UPDATED = 'pgrid:columns-updated'
export const EVENT_ROWS_UPDATED = 'pgrid:rows-updated'
export const EVENT_SORT_CHANGED = 'pgrid:sort-changed'
export const EVENT_CONFIG_CHANGED = 'pgrid:config-changed'
export const DATAFIELD_TOGGLED = 'pgrid:datafield-toggled'

/**
 * Creates a new instance of pgrid
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
export default class Store extends PubSub {

  @observable filters = []
  @observable rows = null
  @observable columns = null
  @observable dataMatrix = {}
  @observable config = {}
  // @observable filteredDataSource = null

  constructor (config) {
    // inherit PubSub
    super()
    this.defaultfield = { name: '#undefined#' }
    this.config = new Config(config)
    this.filters = this.config.getPreFilters()
    this.filteredDataSource = this.config.dataSource

    // this.dataSourceMap = this.buildDataSourceMap()

    this.rows = new Axe(this, AxeType.ROWS)
    this.columns = new Axe(this, AxeType.COLUMNS)
    this.dataMatrix = {}

    // this.query = new Query(this)

    this.refresh()
  }

  refresh (refreshFilters, axe) {
    if (refreshFilters !== false) {
      this.refreshFilteredDataSource()
    }
    switch (axe) {
      case 1: // columns
        this.columns.update()
        this.publish(EVENT_COLUMNS_UPDATED)
        break
      case 2: // rows
        this.rows.update()
        this.publish(EVENT_ROWS_UPDATED)
        break
      // filter updated is not in an axe
      case null:
        break
      case -1: // both axes
      default:
        this.rows.update()
        this.columns.update()
        this.publish(EVENT_UPDATED)
        break
    }
  }

  refreshDataFields () {
    switch (this.config.dataHeadersLocation) {
      case 'rows':
        this.rows.update()
        break
      case 'columns':
        this.columns.update()
        break
      default:
        console.error(`pgrid.config.dataHeadersLocation should be 'rows' or 'columns', instead is ${this.config.dataHeadersLocation}`)
        break
    }
    this.publish(DATAFIELD_TOGGLED)
  }

  // buildDataSourceMap(){
  //   var dimMap = this.config.allFields.reduce(
  //     (myMap, curr) => {
  //       // This is a hacky way to detect which fields are measures in order to avoid mapping them
  //       // This will have to be solved later as part of a bigger overhaul where dimension and measures will be clearly separated
  //       if (curr.aggregateFuncName === null){
  //         myMap[curr.name] = {} }
  //         return myMap
  //       },
  //     {})
  //   dimMap = this.config.dataSource.reduce(
  //     (myMap, curr, currIndex) => {
  //       Object.keys(myMap).forEach(dim => {
  //         if (myMap[dim][curr[dim]] === undefined){myMap[dim][curr[dim]]=[]}
  //         myMap[dim][curr[dim]].push(currIndex)
  //       })
  //       return myMap
  //     }, dimMap)
  //   return dimMap
  //   }

  refreshFilteredDataSource () {
    var filterFields = utils.ownProperties(this.filters)
    if (filterFields.length > 0) {
      this.filteredDataSource = []

      for (var i = 0; i < this.config.dataSource.length; i++) {
        var row = this.config.dataSource[i]
        var exclude = false
        for (var fi = 0; fi < filterFields.length; fi++) {
          var fieldname = filterFields[fi]
          var fieldFilter = this.filters[fieldname]

          if (fieldFilter && !fieldFilter.test(row[fieldname])) {
            exclude = true
            break
          }
        }
        if (!exclude) {
          this.filteredDataSource.push(row)
        }
      }
    } else {
      this.filteredDataSource = this.config.dataSource
    }
  }

  sort (axetype, field) {
    if (axetype === AxeType.ROWS) {
      this.rows.sort(field)
    } else if (axetype === AxeType.COLUMNS) {
      this.columns.sort(field)
    } else {
      return
    }

    this.publish(EVENT_SORT_CHANGED)
  }

  moveField (fieldname, oldaxetype, newaxetype, position) {
    const axe = this.config.moveField(fieldname, oldaxetype, newaxetype, position)
    this.refresh(false, axe)
  }

  toggleDataField (fieldname) {
    if (this.config.toggleDataField(fieldname)) {
      this.refreshDataFields()
      return true
    } else {
      return false
    }
  }

  applyFilter (fieldname, operator, term, staticValue, excludeStatic) {
    this.filters[fieldname] = new ExpressionFilter(operator, term, staticValue, excludeStatic)
    this.refresh()
  }

  refreshData (data) {
    this.config.dataSource = data
    this.refresh()
  }

  toggleSubtotals (axetype) {
    if (this.config.toggleSubtotals(axetype)) {
      this.publish(EVENT_CONFIG_CHANGED)
    }
  }

  toggleGrandtotal (axetype) {
    if (this.config.toggleGrandtotal(axetype)) {
      this.publish(EVENT_CONFIG_CHANGED)
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
    return this.computeValue(rowIndexes, colIndexes, rowIndexes, fieldNames, aggregateFunc)
  }

  getAxisLabel (axisFields) {
    var str = ''
    for (let ti = 0; ti < axisFields.length; ti++) {
      str += (ti > 0 ? ' - ' : '') + axisFields[ti].caption
    }
    return str
  }

  // getChartData() {
  //
  //     var config = this.config
  //
  //
  //     var hAxisLabel = this.getAxisLabel(config.columnFields)
  //     var vAxisLabel = config.dataFields[0].aggregateFuncName + '(' + config.dataFields[0].caption + ')'
  //     var legendsLabel = this.getAxisLabel(config.rowFields)
  //
  //     var rowLeafDimensions = this.rows.flattenValues()
  //     var colLeafDimensions = this.columns.flattenValues()
  //     var data = []
  //
  //     for(var ci=0; ci < colLeafDimensions.length; ci++) {
  //         var cdim = colLeafDimensions[ci]
  //         var currData = [cdim.name]
  //         for(var rri=0; rri < rowLeafDimensions.length; rri++) {
  //             currData.push(this.getData(config.dataFields[0].name, rowLeafDimensions[rri].dim, cdim.dim))
  //         }
  //         data.push(currData)
  //     }
  //
  //     return {
  //         title: vAxisLabel + ': ' + hAxisLabel + ' by ' + legendsLabel,
  //         hAxisLabel: hAxisLabel,
  //         vAxisLabel: vAxisLabel,
  //         legendsLabel: legendsLabel,
  //         colNames: rowLeafDimensions.map(function(d) { return d.name; }),
  //         dataTable: data
  //     }
  // }

  computeValue (rowIndexes, colIndexes, origRowIndexes, fieldNames, aggregateFunc) {
    var res = {}

    if (this.config.dataFieldsCount > 0) {
      var intersection

      if (rowIndexes === null) {
        intersection = colIndexes
      } else if (colIndexes === null) {
        intersection = rowIndexes
      } else {
        intersection = utils.arrayIntersect([colIndexes, rowIndexes])
      }

      var emptyIntersection = intersection && intersection.length === 0
      // var datasource = this.filteredDataSource
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
          res[datafield.field.name] = datafield.aggregateFunc(datafield.field.name, intersection || 'all', this.filteredDataSource, origRowIndexes || rowIndexes, colIndexes)
        }
      }
    }

    return res
  }

  computeRowValues (rowDim) {
    if (rowDim) {
      var data = {}
      var rid = 'r' + rowDim.id

      // set cached row indexes for current row dimension
      if (this._iCache[rid] === undefined) {
        this._iCache[rid] = rowDim.isRoot ? null : (this._iCache[rowDim.parent.id] || rowDim.getRowIndexes())
      }

      // calc grand-total cell
      data[this.columns.root.id] = this.computeValue(rowDim.isRoot ? null : this._iCache[rid].slice(0), null)

      if (this.columns.dimensionsCount > 0) {
        var p = 0
        var parents = [this.columns.root]

        while (p < parents.length) {
          var parent = parents[p]
          var rowindexes = rowDim.isRoot
          ? null
          : (parent.isRoot
             ? this._iCache[rid].slice(0)
              : this._iCache['c' + parent.id].slice(0))

          for (let i = 0; i < parent.values.length; i++) {
            var subdim = parent.subdimvals[parent.values[i]]
            var cid = 'c' + subdim.id

            // set cached row indexes for this column leaf dimension
            if (this._iCache[cid] === undefined) {
              this._iCache[cid] = this._iCache[cid] || subdim.getRowIndexes().slice(0)
            }

            data[subdim.id] = this.computeValue(rowindexes, this._iCache[cid], rowDim.isRoot ? null : rowDim.getRowIndexes())

            if (!subdim.isLeaf) {
              parents.push(subdim)
              if (rowindexes) {
                this._iCache[cid] = []
                for (let ur = 0; ur < rowindexes.length; ur++) {
                  var vr = rowindexes[ur]
                  if (vr !== -1 && vr < 0) {
                    this._iCache[cid].push(0 - (vr + 2))
                    rowindexes[ur] = -1
                  }
                }
              }
            }
          }
          this._iCache['c' + parent.id] = undefined
          p++
        }
      }

      return data
    }
  }

  computeValues () {
    this.dataMatrix = {}
    this._iCache = {}

    // calc grand total row
    // this.dataMatrix[this.rows.root.id] = this.computeRowValues(this.rows.root)

    if (this.rows.dimensionsCount > 0) {
      var parents = [this.rows.root]
      var p = 0
      var parent
      while (p < parents.length) {
        parent = parents[p]
        // calc children rows
        for (let i = 0; i < parent.values.length; i++) {
          var subdim = parent.subdimvals[parent.values[i]]
          // calc child row
          this.dataMatrix[subdim.id] = this.computeRowValues(subdim)
          // if row is not a leaf, add it to parents array to process ijs children
          if (!subdim.isLeaf) {
            parents.push(subdim)
          }
        }
        // next parent
        p++
      }
    }
  }
}
