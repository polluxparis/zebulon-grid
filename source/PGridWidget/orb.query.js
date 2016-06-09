'use strict'

import * as utils from './orb.utils'
import { AxeType } from './orb.axe'
import * as aggregation from './orb.aggregation'

export class Query {

  constructor (source, fieldsConfig) {
    if (utils.isArray(source)) {
      return new ArrayQuery(source).setup(fieldsConfig)
    } else {
      // assume it's a pgrid
      return function (parameters) {
        return new PGridQuery(source).setup(parameters)
      }
    }
  }
}

class QueryBase {

  constructor (source, query, filters) {
    this.source = source
    this.query = query
    this.filters = filters
  }

  extractResult (aggs, options, outerArgs) {
    if (outerArgs.multi === true) {
      var res = {}
      for (let ai = 0; ai < options.multiFieldNames.length; ai++) {
        res[options.multiFieldNames[ai]] = aggs[this.getCaptionName(options.multiFieldNames[ai])]
      }
      return res
    } else {
      return aggs[outerArgs.datafieldname]
    }
  }

  measureFunc (datafieldname, multi, aggregateFunc, fieldsConfig) {
    var outerArgs = {
      datafieldname: this.getCaptionName(datafieldname),
      multi: multi,
      aggregateFunc: aggregateFunc
    }

    return function (options) {
      options = this.cleanOptions(options, arguments, outerArgs)
      var aggs = this.compute(options, fieldsConfig, multi)
      return this.extractResult(aggs, options, outerArgs)
    }
  }

  setDefaultAggFunctions (param) {
    /** *******************
     * val() function    *
     *********************/
    // if there is a registered field with a name or caption 'val', use 'val_'
    var valname = this.query.val ? 'val_' : 'val'
    this.query[valname] = this.measureFunc(undefined, true, undefined, param)

    /** *******************
     * sum(), avg(), ... *
     *********************/
    var aggFunctions = utils.ownProperties(aggregation)
    for (let funcIndex = 0; funcIndex < aggFunctions.length; funcIndex++) {
      var funcName = aggFunctions[funcIndex]
      if (funcName !== 'toAggregateFunc') {
        this.query[funcName] = this.measureFunc(
          undefined,
          true,
          aggregation[funcName],
          param
        )
      }
    }
  }

}

class PGridQuery extends QueryBase {

  constructor (pgrid) {
    super(pgrid, {}, {})
  }

  getCaptionName (caption) {
    return this.source.config.captionToName(caption)
  }

  cleanOptions (options, innerArgs, outerArgs) {
    var opts = {
      fieldNames: [],
      aggregateFunc: null,
      multiFieldNames: []
    }

    if (outerArgs.multi === true) {
      if (options && typeof options === 'object') {
        opts.aggregateFunc = options.aggregateFunc
        opts.multiFieldNames = options.fields
      } else {
        opts.aggregateFunc = outerArgs.aggregateFunc
        opts.multiFieldNames = innerArgs
      }

      for (let ai = 0; ai < opts.multiFieldNames.length; ai++) {
        opts.fieldNames.push(this.getCaptionName(opts.multiFieldNames[ai]))
      }
    } else {
      opts.aggregateFunc = options
      opts.fieldNames.push(outerArgs.datafieldname)
    }

    if (opts.aggregateFunc) {
      opts.aggregateFunc = aggregation.toAggregateFunc(opts.aggregateFunc)
    }

    return opts
  }

  setup (parameters) {
    var rowFields = this.source.config.rowFields
    var colFields = this.source.config.columnFields
    var datafields = this.source.config.dataFields
    var fIndex

    // row fields setup
    for (fIndex = 0; fIndex < rowFields.length; fIndex++) {
      this.slice(rowFields[fIndex], AxeType.ROWS, rowFields.length - fIndex)
    }

    // column fields setup
    for (fIndex = 0; fIndex < colFields.length; fIndex++) {
      this.slice(colFields[fIndex], AxeType.COLUMNS, colFields.length - fIndex)
    }

    // data fields setup
    for (fIndex = 0; fIndex < datafields.length; fIndex++) {
      var df = datafields[fIndex]
      var dfname = df.name
      var dfcaption = df.caption || dfname

      this.query[dfname] = this.query[dfcaption] = super.measureFunc(dfname)
    }

    if (parameters) {
      for (let param in parameters) {
        if (parameters.hasOwnProperty(param)) {
          this.query[param](parameters[param])
        }
      }
    }

    super.setDefaultAggFunctions()

    return this.query
  }

  slice (field, axetype, depth) {
    this.query[field.name] = this.query[field.caption || field.name] = function (val) {
      var f = {
        name: field.name,
        val: val,
        depth: depth
      }(this.filters[axetype] = this.filters[axetype] || []).push(f)
      return this.query
    }
  }

  filterDimensions (upperDims, filter) {
    return function (dim) {
      return dim.value === filter.val &&
      (!upperDims || upperDims.some(
        function (upperDim) {
          var parent = dim.parent
          if (parent) {
            while (parent.depth < upperDim.depth) {
              parent = parent.parent
            }
          }
          return parent === upperDim
        }))
    }
  }

  applyFilters (axetype) {
    if (this.filters[axetype]) {
      var sortedFilters = this.filters[axetype].sort(function (f1, f2) {
        return f2.depth - f1.depth
      })

      var currAxe = this.source[axetype === AxeType.ROWS ? 'rows' : 'columns']
      var filterIndex = 0
      var filtered = null
      while (filterIndex < sortedFilters.length) {
        var filter = sortedFilters[filterIndex]
        filtered = currAxe.dimensionsByDepth[filter.depth]
          .filter(this.filterDimensions(filtered, filter))
        filterIndex++
      }
      return filtered
    }
    return null
  }

  compute (options) {
    var rowdims = this.applyFilters(AxeType.ROWS) || [this.source.rows.root]
    var coldims = this.applyFilters(AxeType.COLUMNS) || [this.source.columns.root]

    var aggs

    if (rowdims.length === 1 && coldims.length === 1) {
      aggs = {}
      for (let ai = 0; ai < options.fieldNames.length; ai++) {
        aggs[options.fieldNames[ai]] = this.source.getData(options.fieldNames[ai], rowdims[0], coldims[0], options.aggregateFunc)
      }
    } else {
      var rowIndexes = []
      var colIndexes = []

      for (let rdi = 0; rdi < rowdims.length; rdi++) {
        rowIndexes = rowIndexes.concat(rowdims[rdi].getRowIndexes())
      }
      for (let cdi = 0; cdi < coldims.length; cdi++) {
        colIndexes = colIndexes.concat(coldims[cdi].getRowIndexes())
      }

      aggs = this.source.calcAggregation(rowIndexes, colIndexes, options.fieldNames, options.aggregateFunc)
    }

    return aggs
  }
}

class ArrayQuery extends QueryBase {

  constructor (array) {
    super(array, {}, [])
    this.captionToName = {}
  }

  setCaptionName (caption, name) {
    this.captionToName[caption || name] = name
  }

  getCaptionName (caption) {
    return this.captionToName[caption] || caption
  }

  cleanOptions (options, innerArgs, outerArgs) {
    var opts = {
      fieldNames: [],
      aggregateFunc: null,
      multiFieldNames: []
    }

    if (outerArgs.multi === true) {
      if (options && typeof options === 'object') {
        opts.aggregateFunc = options.aggregateFunc
        opts.multiFieldNames = options.fields
      } else {
        opts.aggregateFunc = outerArgs.aggregateFunc
        opts.multiFieldNames = innerArgs
      }

      for (let ai = 0; ai < opts.multiFieldNames.length; ai++) {
        opts.fieldNames.push(this.getCaptionName(opts.multiFieldNames[ai]))
      }
    } else {
      opts.aggregateFunc = options || outerArgs.aggregateFunc
      opts.fieldNames.push(outerArgs.datafieldname)
    }

    return opts
  }

  setup (fieldsConfig) {
    this.query.slice = function (field, val) {
      var f = {
        name: field,
        val: val
      }
      this.filters.push(f)
      return this.query
    }

    if (fieldsConfig) {
      var fieldNames = utils.ownProperties(fieldsConfig)

      for (let fi = 0; fi < fieldNames.length; fi++) {
        var fname = fieldNames[fi]
        var f = fieldsConfig[fname]
        var fcaption = f.caption || f.name
        f.name = fname

        this.setCaptionName(fcaption, fname)

        if (f.toAggregate) {
          this.query[fname] = this.query[fcaption] = super.measureFunc(fname, false, f.aggregateFunc)
        } else {
          this.slice(f)
        }
      }
    }

    super.setDefaultAggFunctions(fieldsConfig)

    return this.query
  }

  slice (field) {
    this.query[field.name] = this.query[field.caption || field.name] = function (val) {
      return this.query.slice(field.name, val)
    }
  }

  applyFilters () {
    var rowIndexes = []

    for (let i = 0; i < this.source.length; i++) {
      var row = this.source[i]
      var include = true
      for (let j = 0; j < this.filters.length; j++) {
        var filter = this.filters[j]
        if (row[filter.name] !== filter.val) {
          include = false
          break
        }
      }
      if (include) {
        rowIndexes.push(i)
      }
    }

    return rowIndexes
  }

  compute (options, fieldsConfig, multi) {
    var rowIndexes = this.applyFilters()

    var aggs = {}

    for (let ai = 0; ai < options.fieldNames.length; ai++) {
      var datafield = options.fieldNames[ai]
      var aggFunc = aggregation.toAggregateFunc(
        multi === true
        ? options.aggregateFunc || (fieldsConfig && fieldsConfig[datafield]
          ? fieldsConfig[datafield].aggregateFunc
          : undefined)
          : options.aggregateFunc)

      aggs[datafield] = aggFunc(datafield, rowIndexes || 'all', this.source, rowIndexes, null)
    }

    return aggs
  }
}
