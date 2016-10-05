'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Axe = exports.AxeType = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _Utils = require('./Utils');

var utils = _interopRequireWildcard(_Utils);

var _Dimension = require('./Dimension');

var _Dimension2 = _interopRequireDefault(_Dimension);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Axe types
 * @readonly
 * @enum {Number}
 */
var AxeType = exports.AxeType = {
  COLUMNS: 1,
  ROWS: 2,
  DATA: 3,
  FIELDS: 4,
  CHART: 5
};

/**
 * Creates a new instance of an axe's dimensions list.
 * @class
 * @memberOf orb
 * @param  {array} store - Parent pivot grid
 * @param  {orb.axe.Type} type - Axe type (rows, columns, data)
 */

var Axe = exports.Axe = function () {

  /**
   * Dimensions dictionary indexed by depth
   * @type {Object} Dictionary of (depth, arrays)
   */
  // this.dimensionsByDepth = null

  function Axe(type, fields, store) {
    var _this = this;

    _classCallCheck(this, Axe);

    /**
     * Parent pivot grid
     * @type {orb.store}
     */
    this.store = store;

    /**
     * Axe type (rows, columns, data)
     * @type {orb.axe.Type}
     */
    this.type = type;

    /**
     * This axe dimension fields
     * @type {Array}
     */
    this.fields = fields;

    /**
     * Number of dimensions in this axe
     * @type {Number}
     */
    this.dimensionsCount = this.fields.length;

    /**
     * Root dimension
     * @type {orb.dimension}
     */
    this.root = new _Dimension2.default(-1, null, null, null, this.dimensionsCount + 1, true, false);
    this.fill(this.store.filteredData);
    // initial sort
    this.fields.forEach(function (field) {
      return field.sort.order === 'asc' || field.sort.order === 'desc' ? _this.sort(field, true) : null;
    });
  }

  _createClass(Axe, [{
    key: 'sort',
    value: function sort(field, donottoggle) {
      if (field != null) {
        if (donottoggle !== true) {
          if (field.sort.order !== 'asc') {
            field.sort.order = 'asc';
          } else {
            field.sort.order = 'desc';
          }
        }

        var depth = this.dimensionsCount - this.getfieldindex(field);
        var parents = depth === this.dimensionsCount ? [this.root] : this.getDimensionsByDepth(depth + 1);
        for (var i = 0; i < parents.length; i++) {
          if (field.sort.customfunc != null) {
            parents[i].values.sort(field.sort.customfunc);
          } else {
            parents[i].values.sort();
          }
          if (field.sort.order === 'desc') {
            parents[i].values.reverse();
          }
        }
      }
    }

    // perhaps introduce a result parameter to obtain tail call optimisation

  }, {
    key: 'getDimensionsByDepth',
    value: function getDimensionsByDepth(depth, dim) {
      var _ref,
          _this2 = this;

      if (!dim) {
        dim = this.root;
      }
      if (depth === this.dimensionsCount + 1) {
        return [dim];
      }
      return (_ref = []).concat.apply(_ref, _toConsumableArray(Object.keys(dim.subdimvals).map(function (dimValue) {
        return _this2.getDimensionsByDepth(depth + 1, dim.subdimvals[dimValue]);
      })));
    }
  }, {
    key: 'getfieldindex',
    value: function getfieldindex(field) {
      return this.fields.map(function (fld) {
        return fld.name;
      }).indexOf(field.name);
    }

    /**
     * Creates all subdimensions using the supplied data
     * fill does two things:
     *   - filling the dimensionsByDepth array of the axe (used for sorting and flattenValues - note sure if useful)
     *   - filling the subdimvals array of each dimension of the axe
     *   - filling the rowIndexes array of each dimension of the axe (used for calculating aggregations)
     */

  }, {
    key: 'fill',
    value: function fill(data) {
      if (data != null && this.dimensionsCount > 0) {
        if (data != null && utils.isArray(data) && data.length > 0) {
          for (var rowIndex = 0, dataLength = data.length; rowIndex < dataLength; rowIndex++) {
            var row = data[rowIndex];
            var dim = this.root;
            for (var findex = 0; findex < this.dimensionsCount; findex++) {
              var depth = this.dimensionsCount - findex;
              var field = this.fields[findex];
              var subvalue = row[field.name];
              var id = row[field.code];
              var subdimvals = dim.subdimvals;
              if (subdimvals[subvalue] !== undefined) {
                dim = subdimvals[subvalue];
              } else {
                dim.values.push(subvalue);
                dim = new _Dimension2.default(id, dim, subvalue, field, depth, false, findex === this.dimensionsCount - 1);
                subdimvals[subvalue] = dim;
                dim.rowIndexes = [];
              }
              dim.rowIndexes.push(rowIndex);
            }
          }
        }
      }
    }

    // flattenValues() {
    //     return this.dimensionsByDepth[1].map(function(dim) {
    //         var name = ''
    //         var currDim = dim
    //         while(!currDim.isRoot) {
    //             name = currDim.value + (name !== '' ? '-' + name : '')
    //             currDim = currDim.parent
    //         }
    //         return {
    //             name: name,
    //             dim: dim
    //         }
    //     }).sort(function(a, b) {
    //         if(a.name < b.name) return -1
    //         if(a.name > b.name) return 1
    //         return 0
    //     })
    // }

  }]);

  return Axe;
}();